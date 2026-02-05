BeginPackage["QED`Numeric`", {"QED`Numeric`HarmonicOscillator`"}];

Needs["QED`Model`"];

(* Экспорт символов *)
PrepareNumericModel::usage = "PrepareNumericModel[symModel, params] prepares numeric functions.";
ComputeEvolution::usage = "ComputeEvolution[model, tmax] computes NDSolve solution.";

FindPotentialMinimum::usage = "FindPotentialMinimum[hamiltonian, topology, substitutionRules] \
numerically finds equilibrium flux values φ_min that minimize potential energy U(φ). \
Uses PrincipalAxis method (gradient-free local optimization) starting from external flux φ_ext. \
Optimized for smooth potentials with good initial guess (~0.002 sec). \
Returns substitution rules: {φ₁ -> value₁, φ₂ -> value₂, ...} in Weber.";

FindEquilibriumPoints::usage = "FindEquilibriumPoints[hamiltonian, topology, substitutionRules] \
finds all equilibrium flux configurations by solving ∇U = 0 on a grid of starting points. \
Returns Association with list of solutions, energies, and residuals.";

FindPotentialMinimumContinuation::usage = 
  "FindPotentialMinimumContinuation[gradient, hessian, fluxVars, topology, phiExtTarget, opts] \
finds equilibrium flux using homotopy continuation with pre-cached symbolic derivatives. \
Requires rescaled derivatives: gradient = D[U(φ̃*Φ₀), φ̃], hessian = D²[U(φ̃*Φ₀), φ̃²]. \
Options: \"StepSize\" (0.05 Φ₀), \"MaxSteps\" (100), \"Tolerance\" (10^-10). \
Performance: ~1 ms per call (vs 4 ms with on-the-fly differentiation).";

FindPotentialMinimumContinuation::badstep = 
  "Continuation failed at step `1` of `2`. Try reducing StepSize option.";

ComputeNormalModeFrequencies::usage = "ComputeNormalModeFrequencies[invCap, L] \
computes normal mode frequencies ω_i from eigenvalues of C^(-1)·L matrix. \
Returns frequencies in rad/s (SI units), sorted by increasing frequency.";

PlasmonFrequenciesVsFlux::usage = 
  "PlasmonFrequenciesVsFlux[model] возвращает численную функцию ω[φext_?NumericQ], \
где φext в единицах Φ₀. Возвращает список частот {ω₁, ω₂, ...} в rad/s.";

VerifyWaveFunction::usage = "VerifyWaveFunction[model, state] verifies that H_harm|psi> = E_harm|psi>.";

VerifyDiagonalization::usage = "VerifyDiagonalization[model] numerically checks if the calculated \
FluxTransform matrix correctly diagonalizes both Capacitance and Inductance matrices. \
Returns <|'Is_L_Diagonal', 'Is_C_Diagonal', ...|>.";

CreateAnnihilationMatrix::usage = 
"CreateAnnihilationMatrix[dim] returns a sparse matrix (dim x dim) for the annihilation operator.
Matrix elements: <n-1|a|n> = Sqrt[n].";

EmbedOperator::usage = 
"EmbedOperator[op, modeIndex, dimensions] computes the Kronecker product 
to embed a single-mode operator 'op' into the full Hilbert space defined by 'dimensions'.
Example: EmbedOperator[a, 2, {dim1, dim2, dim3}] -> I_1 \[KroneckerProduct] a_2 \[KroneckerProduct] I_3";

GetBasisOperators::usage = 
"GetBasisOperators[dimensions] returns an Association containing the annihilation ('a') 
and creation ('ad') operators for each mode, embedded in the full Hilbert space.
Input: dimensions = {dim_1, dim_2, ...} (truncation levels for each mode).
Output: <| \"a\" -> {A_1, A_2, ...}, \"ad\" -> {Ad_1, Ad_2, ...}, \"Identity\" -> I_total |>";

ConstructFluxOperators::usage = 
"ConstructFluxOperators[model, basisOps] constructs the flux operators for each node in the laboratory frame.
Returns a list of SparseArray matrices {Phi_1, Phi_2, ...} corresponding to the nodes.
Requires 'HarmonicDiagonalization' to be present in the model.";

Begin["`Private`"];



Options[FindPotentialMinimumContinuation] = {
  "StepSize" -> 0.05,          (* Δφ в единицах Φ₀ *)
  "MaxSteps" -> 100,           (* защита от бесконечного цикла *)
  "Tolerance" -> 10^-8         (* точность FindRoot *)
};

Options[FindEquilibriumPoints] = {
  GridResolution -> 5,
  MaxResidual -> 10^-5,
  Method -> "Newton"
};

$DebugFindPotentialMinimumContinuation = False;
$DebugFindPotentialMinimum = False;
$DebugFindEquilibriumPoints = False;
$DebugPlasmonFrequencies = False;

(*
  Physics: Find equilibrium positions φ_min where ∂U/∂φ = 0.
  
  For flux-biased circuits (qubits, tunable couplers), the equilibrium 
  depends on external flux Φ_ext. 
  
  Algorithm with dimensionless rescaling for numerical stability:
  1. Extract potential energy U(φ) by setting all charges q_i = 0
  2. Rescale energy by EJ: Ũ = U/EJ (dimensionless)
  3. Rescale fluxes by Φ₀: φ̃ = φ/Φ₀ (dimensionless)
  4. Minimize Ũ(φ̃) using global RandomSearch + local QuasiNewton
  5. Convert back: φ = φ̃ * Φ₀
  
  UPDATE: Replaced FindMinimum (local) with NMinimize/RandomSearch (global)
  for multi-well potentials (e.g., bridge flux qubit at Φext ~ 0.5Φ₀).
  
  Reference: Manucharyan et al., Science 326, 113 (2009), Fig. 2
*)

FindPotentialMinimum[hamiltonian_, topology_Association, substitutionRules_List] := 
 Module[{nodes, fluxVars, potential, potentialNumeric, externalFlux, 
         energyScale, potentialRescaled, externalFluxRescaled,
         constraints, result, minValues, phi0Value, minSymbols},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Потенциальная энергия: U(φ) = H(q=0, φ) *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Константы *)
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;

  If[$DebugFindPotentialMinimum === True,
    Print["[DEBUG FindPotentialMinimum]"];
    Print["  PhiExt from rules: ", externalFlux];
    Print["  PhiExt / Phi_0: ", N[externalFlux / phi0Value, 3]];
  ];

  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;

  If[$DebugFindPotentialMinimum === True,
    Print["Potential after substitution: ", Short[potentialNumeric, 3]];
    Print["Contains $CurrentModel? ", !FreeQ[potentialNumeric, $CurrentModel]];
    Print["Contains symbols? ", Cases[potentialNumeric, _Symbol, {0, 5}]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ОБЕЗРАЗМЕРИВАНИЕ для численной стабильности                      *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Извлечь EJ из коэффициента при Cos *)
  energyScale = Abs @ First @ Cases[
    potentialNumeric,
    c_?NumericQ * Cos[_] :> c,
    Infinity
  ];
  
  If[!NumericQ[energyScale] || energyScale == 0,
    Print["Warning: Cannot extract energy scale. Using 1."];
    energyScale = 1;
  ];
  
  (* Обезразмерить: Ũ = U/EJ, φ̃ = φ/Φ₀ *)
  potentialRescaled = (potentialNumeric / energyScale) /. 
    Thread[fluxVars -> fluxVars * phi0Value];
  externalFluxRescaled = externalFlux / phi0Value;
  
  (* Отладочный вывод *)
  If[$DebugFindPotentialMinimum === True,
    Print["Rescaled potential: ", Simplify[potentialRescaled]];
    Print["External flux (rescaled): ", externalFluxRescaled];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ГЛОБАЛЬНАЯ минимизация: RandomSearch + QuasiNewton              *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Ограничения: поиск в области [-0.5, 0.5] для безразмерных фаз *)
  constraints = Thread[-0.5 < fluxVars < 0.5];
  
  result = Quiet[
    NMinimize[
      {potentialRescaled, constraints},
      fluxVars,
      Method -> {
        "RandomSearch", 
        "SearchPoints" -> 10,        (* 30 случайных стартовых точек *)
        "RandomSeed" -> 12345,       (* воспроизводимость *)
        "PostProcess" -> {           (* локальная доводка *)
          "FindMinimum",
          Method -> "QuasiNewton"
        }
      },
      MaxIterations -> 3,
      AccuracyGoal -> 6,
      PrecisionGoal -> 6
    ],
    {NMinimize::cvmit, NMinimize::nosat, FindMinimum::lstol, FindMinimum::sdprec}
  ];
  
  (* Отладочный вывод *)
  If[$DebugFindPotentialMinimum === True,
    If[result =!= $Failed && NumericQ[result[[1]]],
      Print["Minimum energy (rescaled): ", result[[1]]];
      Print["Minimum energy (physical): ", result[[1]] * energyScale, " J"];
    ];
  ];
  
  (* DEBUG: два минимума *)
  If[$DebugFindPotentialMinimum === True,
    Module[{res1, res2, E1, E2, phi1, phi2},
      res1 = NMinimize[{potentialRescaled, constraints}, fluxVars, 
        Method -> {"RandomSearch", "SearchPoints" -> 10}];
      res2 = NMinimize[{potentialRescaled, constraints}, fluxVars, 
        Method -> {"RandomSearch", "SearchPoints" -> 10, "RandomSeed" -> 999}];
      E1 = res1[[1]]; phi1 = res1[[2]];
      E2 = res2[[1]]; phi2 = res2[[2]];
      Print["E1 = ", ScientificForm[E1, 3], " at φ = ", fluxVars /. phi1];
      Print["E2 = ", ScientificForm[E2, 3], " at φ = ", fluxVars /. phi2];
      Print["ΔE = ", ScientificForm[Abs[E1-E2], 2]];
    ];
  ];

  
  (* DEBUG: градиент в минимуме *)
  If[$DebugFindPotentialMinimum === True && result =!= $Failed,
    Print["∇U = ", D[potentialRescaled, #] & /@ fluxVars /. result[[2]]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ОБРАТНОЕ МАСШТАБИРОВАНИЕ: φ̃ → φ                                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Создать символы для минимума: Subscript[φ, "min", i] *)
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  If[result === $Failed || !NumericQ[result[[1]]],
    (* Fallback: использовать внешний поток как приближение *)
    Print["Warning: Minimization failed. Using φ_min ≈ φ_ext."];
    minValues = Thread[minSymbols -> externalFlux],
    
    (* Успех: конвертировать обратно в Weber *)
    minValues = Thread[minSymbols -> (fluxVars /. result[[2]]) * phi0Value]
  ];
  
  minValues
];


FindEquilibriumPoints[hamiltonian_, gradient_List, topology_Association, 
  substitutionRules_List, opts:OptionsPattern[]] := 
 Module[{nodes, fluxVars, phi0Value, externalFlux, potential, potentialNumeric,
         potentialRescaled, gradientNumeric, gradientRescaled, equationsRescaled, 
         gridResolution, grid1D, gridPoints, rawSolutions, validSolutions, 
         solutions, startTime},
  
  startTime = AbsoluteTime[];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ПОДГОТОВКА ПЕРЕМЕННЫХ                                            *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
  
  (* Потенциальная энергия для вычисления энергий *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  potentialRescaled = potentialNumeric /. Thread[fluxVars -> fluxVars * phi0Value];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["=== FindEquilibriumPoints ==="];
    Print["Variables: ", fluxVars];
    Print["External flux: ", externalFlux];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ОБЕЗРАЗМЕРИВАНИЕ ГРАДИЕНТА                                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  gradientNumeric = gradient /. substitutionRules /. QED`$Phi0 -> phi0Value;
  gradientRescaled = gradientNumeric /. Thread[fluxVars -> fluxVars * phi0Value];
  equationsRescaled = Thread[gradientRescaled == 0];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["Number of equations: ", Length[equationsRescaled]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ГЕНЕРАЦИЯ СЕТКИ СТАРТОВЫХ ТОЧЕК                                  *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  gridResolution = OptionValue[GridResolution];
  grid1D = Subdivide[-0.5, 0.5, gridResolution - 1];
  gridPoints = Tuples[Table[grid1D, {Length[fluxVars]}]];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["Grid: ", gridResolution, "^", Length[fluxVars], 
          " = ", Length[gridPoints], " points"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* РЕШЕНИЕ СИСТЕМЫ ОТ КАЖДОЙ СТАРТОВОЙ ТОЧКИ                        *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  rawSolutions = Table[
    Quiet[
      Check[
        FindRoot[
          equationsRescaled,
          Thread[{fluxVars, gridPoints[[i]]}],
          Method -> OptionValue[Method],
          MaxIterations -> 50
        ],
        $Failed,
        {FindRoot::cvmit, FindRoot::lstol}
      ]
    ],
    {i, Length[gridPoints]}
  ];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["Raw solutions: ", Count[rawSolutions, Except[$Failed]], "/", Length[gridPoints]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ФИЛЬТРАЦИЯ: отбросить $Failed и проверить невязки               *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  validSolutions = Select[rawSolutions, # =!= $Failed &];
  
  (* Проверить невязки |∇U| < threshold *)
  validSolutions = Select[validSolutions,
    Module[{residual},
      residual = Norm[gradientRescaled /. #];
      residual < OptionValue[MaxResidual]
    ] &
  ];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["Valid solutions after residual check: ", Length[validSolutions]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* НОРМАЛИЗАЦИЯ И УДАЛЕНИЕ ДУБЛИКАТОВ                              *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Шаг 1: Нормализовать безразмерные решения к [-0.5, 0.5] *)
  validSolutions = validSolutions /. 
    Rule[var_, val_] :> Rule[var, Mod[val + 0.5, 1.0] - 0.5];
  
  (* Шаг 2: Удалить дубликаты (в безразмерных координатах!) *)
  validSolutions = DeleteDuplicatesBy[validSolutions,
    Round[Values[#], 10^-6] &
  ];
  
  If[$DebugFindEquilibriumPoints === True,
    Print["Unique solutions after normalization: ", Length[validSolutions]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ВЫЧИСЛЕНИЕ ЭНЕРГИЙ И ФОРМИРОВАНИЕ РЕЗУЛЬТАТА                     *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  solutions = Table[
    Module[{sol, energy, residual, fluxesPhysical},
      sol = validSolutions[[i]];
      energy = potentialRescaled /. sol;
      residual = Norm[gradientRescaled /. sol];
      
      (* Конвертировать безразмерные решения в физические единицы Weber *)
      fluxesPhysical = Thread[fluxVars -> (fluxVars /. sol) * phi0Value];
      
      <|
        "Fluxes" -> fluxesPhysical,
        "Energy" -> energy,
        "Residual" -> residual
      |>
    ],
    {i, Length[validSolutions]}
  ];
  
  
If[$DebugFindEquilibriumPoints === True,
  Print["=== EQUILIBRIUM ANALYSIS ==="];
  
  (* Энергии всех решений *)
  Module[{energies, Emin, Emax, dE},
    energies = Sort[#"Energy" & /@ solutions];
    Emin = First[energies];
    Emax = Last[energies];
    dE = Emax - Emin;
    
    Print["Energy range: [", ScientificForm[Emin, 3], ", ", 
          ScientificForm[Emax, 3], "], ΔE = ", ScientificForm[dE, 3]];
    Print["Ground state: E₀ = ", ScientificForm[Emin, 4]];
    Print["Barrier: Umax - E₀ = ", ScientificForm[dE, 3]];
  ];
  
  (* Группировка по энергиям (вырожденность) - НЕ округлять! *)
  Module[{grouped, degeneracies},
    grouped = GroupBy[solutions, Round[#"Energy", 10^-25] &];  (* <-- FIX *)
    degeneracies = Sort[Tally[Length /@ Values[grouped]][[All, 1]], Greater];
    Print["Degeneracies: ", Take[degeneracies, UpTo[5]], " solutions per level"];
  ];
  
  (* Топ-3 минимума *)
  Module[{top3},
    top3 = Take[SortBy[solutions, #"Energy" &], UpTo[3]];
    Print["=== TOP 3 MINIMA ==="];
    MapIndexed[
      Print["#", #2[[1]], ": E = ", ScientificForm[#1["Energy"], 4], 
            ", φ = ", Round[Values[#1["Fluxes"]] / phi0Value, 0.001]] &,
      top3
    ];
  ];
  
  Print["Compute time: ", AbsoluteTime[] - startTime, " sec"];  (* <-- FIX *)
];

  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ФИНАЛЬНЫЙ РЕЗУЛЬТАТ                                              *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  <|
    "Solutions" -> solutions,
    "GridSize" -> gridResolution,
    "NumSolutions" -> Length[solutions],
    "ComputationTime" -> AbsoluteTime[] - startTime
  |>
];


(*
  Physics: Continuation-based equilibrium tracking with pre-cached derivatives.
  
  Algorithm:
  1. Receives pre-computed gradient ∇U and Hessian ∇²U from Model cache
  2. Builds homotopy path: Φext = 0 → target
  3. Newton-Raphson at each step with analytical Jacobian
  
  Performance: ~1 ms per call (vs 4 ms with symbolic differentiation)
  
  Reference: Allgower & Georg, "Numerical Continuation Methods" (1990)
*)

FindPotentialMinimumContinuation[
  gradientRescaled_List,      (* Предвычисленный ∇U(φ̃) *)
  hessianRescaled_List,       (* Предвычисленный ∇²U(φ̃) *)
  fluxVars_List,              (* Список переменных {φ̃₁, φ̃₂, ...} *)
  topology_Association,
  phiExtTarget_?NumericQ,     (* Целевое значение в Weber *)
  opts:OptionsPattern[]
] := Module[{
  nodes, phi0Value, phiExtDimensionless, stepSize, maxSteps,
  nSteps, phiExtPath, initialSolution, solutionPath, finalSolution,
  minSymbols, tolerance, startTime, endTime
  },
  
  startTime = AbsoluteTime[];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 1. PREPARATION                                                   *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  phi0Value = QED`$Phi0Value;
  phiExtDimensionless = phiExtTarget / phi0Value;
  
  If[$DebugFindPotentialMinimumContinuation === True,
    Print["[Continuation] Using pre-cached derivatives"];
    Print["[Continuation] Target Phi_ext = ", Round[phiExtDimensionless, 0.001], " Phi_0"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 2. BUILD PATH                                                    *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  stepSize = OptionValue["StepSize"];
  maxSteps = OptionValue["MaxSteps"];
  tolerance = OptionValue["Tolerance"];
  
  nSteps = Min[Ceiling[Abs[phiExtDimensionless] / stepSize], maxSteps];
  phiExtPath = Subdivide[0.0, phiExtDimensionless, nSteps];
  
  If[$DebugFindPotentialMinimumContinuation === True,
    Print["[Continuation] Steps: ", nSteps, " x ", stepSize, " Phi_0"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 3. FUNCTIONAL LOOP: FoldList                                     *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  initialSolution = Thread[fluxVars -> 0.0];
  
  solutionPath = FoldList[
    Function[{prevSol, phiExtCurrent},
      Module[{gradVec, hessMat, startPoint, newSol, phiExtValue},
        
        phiExtValue = phiExtCurrent * phi0Value;
        
        (* Подставить Φext *)
        gradVec = gradientRescaled /. {QED`$PhiExt -> phiExtValue};
        hessMat = hessianRescaled /. {QED`$PhiExt -> phiExtValue};
        
        startPoint = Thread[{fluxVars, fluxVars /. prevSol}];
        
        (* Минимальный FindRoot *)
        newSol = Quiet[
          Check[
            FindRoot[
              Thread[gradVec == 0],
              startPoint,
              Jacobian -> hessMat
            ],
            $Failed,
            {FindRoot::cvmit, FindRoot::lstol, FindRoot::jsing}
          ],
          {FindRoot::cvmit, FindRoot::lstol, FindRoot::jsing}
        ];
        
        newSol
      ]
    ],
    initialSolution,
    Rest[phiExtPath]
  ];

  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 4. HANDLE FAILURES                                               *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  solutionPath = TakeWhile[solutionPath, # =!= $Failed &];
  
  If[Length[solutionPath] < nSteps + 1,
    Message[FindPotentialMinimumContinuation::badstep,
            Length[solutionPath], nSteps];
    Return[$Failed, Module]
  ];
  
  finalSolution = Last[solutionPath];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 5. FORMAT RESULT                                                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  Module[{values},
    values = (fluxVars /. finalSolution) * phi0Value;
    
    endTime = AbsoluteTime[];
    
    If[$DebugFindPotentialMinimumContinuation === True,
      Print["[Continuation] Total execution time: ", endTime - startTime, " sec"];
    ];
    
    Thread[minSymbols -> values]
  ]
];






(*
  Physics: Normal mode frequencies from harmonic approximation.
  
  For quadratic Hamiltonian H = (1/2) q^T C^(-1) q + (1/2) φ^T L^(-1) φ,
  normal modes satisfy:
  
  ω_i^2 = eigenvalues(C⁻¹ · L⁻¹)
  
  Returns Association with:
  - "Frequencies": ω_i in rad/s (SI units), sorted. Complex if unstable modes exist.
  - "IsStable": True if all ω² > 0 (stable equilibrium)
  - "NumUnstableModes": Count of modes with ω² < 0 (saddle point indicator)
  
  Reference: Devoret lectures, Les Houches (2004), Section 3.3
*)

ComputeNormalModeFrequencies[invCap_?MatrixQ, invInd_?MatrixQ] := Module[
  {omega2, frequencies, threshold = 10^(-10)},

  (* ω² = eigenvalues(C⁻¹ · L⁻¹) *)
  omega2 = Eigenvalues[invCap . invInd];

  (* Вычислить sqrt, для отрицательных → комплексные *)
  frequencies = Sort[Sqrt[omega2 + 0. I], Re[#1] < Re[#2] &];
  
  (* Вернуть с диагностикой *)
  <|
    "Frequencies" -> Chop[frequencies],
    "IsStable" -> AllTrue[omega2, # > threshold &],
    "NumUnstableModes" -> Count[omega2, x_ /; x < -threshold]
  |>
];



(*
  Physics: Plasmon frequencies as function of external flux.
  
  Returns pure function ω[φext_?NumericQ] where φext is dimensionless (in Φ₀ units).
  For each flux value, performs:
  1. FindPotentialMinimumContinuation to find equilibrium (fast: ~1ms)
  2. Eigenvalue decomposition of C⁻¹·L⁻¹
  3. Returns sorted frequencies ω_i in rad/s
  
  Performance: Uses continuation method by default (~43x faster than global search).
  Fallback: If continuation derivatives unavailable, returns $Failed.
  
  Reference: Koch et al., PRA 76, 042319 (2007), Eq. 8
*)


PlasmonFrequenciesVsFlux[model_Association] := Module[
  {
    capSym, lindInvSym, topology, rulesBase, phiExtSym, phi0,
    gradientRescaled, hessianRescaled, fluxVars, nodes,
    callCounter = 0, totalContinuationTime = 0, totalEigenTime = 0, totalOverhead = 0,
    useContinuation
  },
  
  (* Аналитические матрицы из модели *)
  capSym     = model["Analytical"]["CapacitanceMatrix"];
  lindInvSym = model["Analytical"]["InductanceMatrix"];  (* L⁻¹ *)
  topology   = model["Topology"];  

  
  (* Физические константы *)
  phiExtSym = QED`$PhiExt;
  phi0      = QED`$Phi0Value;
  
  (* Извлечь узлы и переменные *)
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Базовые правила подстановки БЕЗ внешнего потока и φ_min *)
  rulesBase = DeleteCases[
    model["SubstitutionRules"],
    (phiExtSym :> _) | (Subscript[QED`$FluxSymbol, "min", _] :> _)
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ИЗВЛЕЧЕНИЕ CONTINUATION DERIVATIVES ИЗ КЭША                      *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  useContinuation = KeyExistsQ[model, "Numerical"] && 
                    KeyExistsQ[model["Numerical"], "Cache"] &&
                    KeyExistsQ[model["Numerical"]["Cache"], "ContinuationDerivatives"];
  
  If[useContinuation,
    Module[{cache},
      cache = model["Numerical"]["Cache"]["ContinuationDerivatives"];
      gradientRescaled = Lookup[cache, "Gradient", $Failed];
      hessianRescaled = Lookup[cache, "Hessian", $Failed];
      
      If[gradientRescaled === $Failed || hessianRescaled === $Failed,
        useContinuation = False;
        Print["[WARNING] Continuation derivatives not found in cache. This should not happen!"];
      ];
    ];
  ];
  
  If[!useContinuation,
    Print["[ERROR] PlasmonFrequenciesVsFlux requires continuation derivatives in model cache."];
    Print["[ERROR] Make sure ComputeNumericalHarmonicPerturbation has been called."];
    Return[$Failed];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ВОЗВРАЩАЕМ ЧИСЛЕННУЮ ФУНКЦИЮ С ПРОФИЛИРОВАНИЕМ                   *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Function[{phiExtDimensionless},
    Module[{phiExtPhysical, rulesWithFlux, capNum, lindInvNum, 
            invCapNum, omega2, frequencies, equilibriumRules,
            tStart, tAfterContinuation, tAfterEigen, tEnd},
      
      If[!NumericQ[phiExtDimensionless],
        Return[$Failed, Module]
      ];

      tStart = AbsoluteTime[];
      callCounter++;
      
      (* Конвертировать φext из единиц Φ₀ в Weber *)
      phiExtPhysical = phiExtDimensionless * phi0;
      
      (* Подставить текущее значение внешнего потока *)
      rulesWithFlux = Append[rulesBase, phiExtSym -> phiExtPhysical];
      
      (* ════════════════════════════════════════════════════════════════ *)
      (* ПРОФИЛИРОВАНИЕ: FindPotentialMinimumContinuation                 *)
      (* ════════════════════════════════════════════════════════════════ *)
      
      equilibriumRules = FindPotentialMinimumContinuation[
        gradientRescaled,
        hessianRescaled,
        fluxVars,
        topology,
        phiExtPhysical
      ];

      tAfterContinuation = AbsoluteTime[];
      
      rulesWithFlux = Join[rulesWithFlux, equilibriumRules];
      
      If[$DebugPlasmonFrequencies === True,
        Print["lindInvSym before substitution:"];
        Print[Short[lindInvSym, 2]];
        Print["Contains φ_min? ", !FreeQ[lindInvSym, Subscript[QED`$FluxSymbol, "min", _]]];
        Print["Contains PhiExt? ", !FreeQ[lindInvSym, QED`$PhiExt]];
      ];

      (* Численные матрицы *)
      capNum     = capSym //. rulesWithFlux;
      lindInvNum = lindInvSym //. rulesWithFlux;
      
      If[$DebugPlasmonFrequencies === True,
        Print["lindInvNum after substitution:"];
        Print[Short[lindInvNum, 2]];
        Print["Contains symbols? ", !FreeQ[lindInvNum, _Symbol]];
        Print["Numerical? ", MatrixQ[lindInvNum, NumericQ]];
      ];

      (* C⁻¹ *)
      If[Det[capNum] == 0, Return[$Failed]];
      invCapNum = Inverse[capNum];
      
      (* ════════════════════════════════════════════════════════════════ *)
      (* ПРОФИЛИРОВАНИЕ: Eigenvalue computation                           *)
      (* ════════════════════════════════════════════════════════════════ *)
      
      (* ω² = eigenvalues(C⁻¹ · L⁻¹) *)
      omega2 = Eigenvalues[N[invCapNum . lindInvNum]];
      
      tAfterEigen = AbsoluteTime[];
      
      (* √ω² с сортировкой, комплексные если неустойчивость *)
      frequencies = Sort[Sqrt[omega2 + 0. I], Re[#1] < Re[#2] &];
      
      tEnd = AbsoluteTime[];
      
      
      (* ════════════════════════════════════════════════════════════════ *)
      (* ПРОФИЛИРОВАНИЕ: Накопление статистики                            *)
      (* ════════════════════════════════════════════════════════════════ *)
      If[$DebugPlasmonFrequencies === True,
        Module[{dtContinuation, dtEigen, dtOverhead, dtTotal},
          dtContinuation = (tAfterContinuation - tStart) * 1000;
          dtEigen = (tAfterEigen - tAfterContinuation) * 1000;
          dtTotal = (tEnd - tStart) * 1000;
          dtOverhead = dtTotal - dtContinuation - dtEigen;
          
          totalContinuationTime += dtContinuation;
          totalEigenTime += dtEigen;
          totalOverhead += dtOverhead;
          
          
          (* Вывод для первой и каждой 10-й точки *)
          If[callCounter == 1 || Mod[callCounter, 10] == 0,
            Print["[PROFILE Point ", callCounter, "]"];
            Print["  Continuation: ", Round[dtContinuation, 0.1], " ms"];
            Print["  Eigenvalues: ", Round[dtEigen, 0.1], " ms"];
            Print["  Overhead: ", Round[dtOverhead, 0.1], " ms"];
            Print["  Total: ", Round[dtTotal, 0.1], " ms"];
          ];
          
          (* Итоговый отчёт после 25 и 50 вызовов *)
          If[callCounter > 20 && Mod[callCounter, 25] == 0,
            Print[""];
            Print["[PROFILE SUMMARY after ", callCounter, " calls]"];
            Print["  Continuation: ", Round[totalContinuationTime, 0.1], " ms (", 
                  Round[100 * totalContinuationTime / (totalContinuationTime + totalEigenTime + totalOverhead), 1], "%)"];
            Print["  Eigenvalues: ", Round[totalEigenTime, 0.1], " ms (", 
                  Round[100 * totalEigenTime / (totalContinuationTime + totalEigenTime + totalOverhead), 1], "%)"];
            Print["  Overhead: ", Round[totalOverhead, 0.1], " ms (", 
                  Round[100 * totalOverhead / (totalContinuationTime + totalEigenTime + totalOverhead), 1], "%)"];
            Print["  TOTAL: ", Round[totalContinuationTime + totalEigenTime + totalOverhead, 0.1], " ms"];
            Print["  Average per point: ", Round[(totalContinuationTime + totalEigenTime + totalOverhead) / callCounter, 0.1], " ms"];
          ];
        ];
      ];

      Chop[frequencies]
    ]
  ]
];

(* ════════════════════════════════════════════════════════════════ *)
(*                       DEBUG FLAG                                 *)
(* ════════════════════════════════════════════════════════════════ *)
  

PrepareNumericModel[symModel_Association, params_Association] := Module[{sol},
  (* подготовка численных функций из символики *)
  sol
];

ComputeEvolution[model_, tmax_?NumericQ] := Module[{sol},
  (* NDSolve *)
  sol
];


(* Извлечь значение из записи кэша, вычисляя если нужно *)
GetCacheEntry[cacheEntry_Association, model_Association] := Module[
  {state, thunk},
  
  state = Lookup[cacheEntry, "State", "Unknown"];
  
  Which[
    state === "Ready",
      Lookup[cacheEntry, "Value", $Failed],
    
    state === "Lazy",
      thunk = Lookup[cacheEntry, "Thunk", $Failed];
      If[thunk === $Failed, $Failed, thunk[model]],
    
    True,
      $Failed
  ]
];

cleanExpr[expr_] := expr /. {Abs'[x_] :> Sign[x], Conjugate'[x_] :> Conjugate[x]};

(*
  VerifyWaveFunction:
  
  Verifies that the HARMONIC Hamiltonian H_harm (with substituted parameters) 
  satisfies H_harm|psi> = E_harm|psi> when charges are replaced by derivatives.
  
  NOTE: This uses the harmonic approximation Hamiltonian, not the full non-linear one,
  because the wavefunctions are eigenstates of the harmonic oscillator.
*)

VerifyWaveFunction[model_Association, state_List] := Block[
  {QED`Model`$CurrentModel = model},
  Module[{
    hamSym, subRules, eqFluxes, hamNum,
    topology, nodes, fluxVars, 
    potentialNumeric, kineticNumeric,
    diagData, omegas, hbarValue = QED`$hbarValue,
    psi, constTerm, energyVal,
    hPsi, ePsi, residual, phiToMinVal, normVal
  },

    (* 1. Get Symbolic Harmonic Hamiltonian *)
    hamSym = model["Analytical"]["HarmonicHamiltonian"];
    If[MissingQ[hamSym], Return[<|"Status" -> "FAIL", "Reason" -> "HarmonicHamiltonianMissing"|>]];

    (* 2. Get Substitution Rules and Equilibrium Fluxes *)
    subRules = model["SubstitutionRules"];
    eqFluxes = QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
    If[FailureQ[eqFluxes], Return[<|"Status" -> "FAIL", "Reason" -> "EquilibriumFluxesMissing"|>]];
    
    (* 3. Substitute to get Numerical Harmonic Hamiltonian *)
    (* Note: eqFluxes rules replace Subscript[φ, "min", i] which appear in H_harm *)
    hamNum = hamSym /. subRules /. eqFluxes;

    (* 4. Extract Topology Variables *)
    topology = model["Topology"];
    nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
    fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
    
    (* 5. Separate Kinetic and Potential parts *)
    (* Potential: set charges to 0 *)
    potentialNumeric = hamNum /. Subscript[QED`$ChargeSymbol, _] -> 0;
    
    (* Kinetic: subtract potential from total *)
    kineticNumeric = hamNum - potentialNumeric;

    (* 6. Get Diagonalization Data (for omegas) *)
    diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
    If[FailureQ[diagData], Return[<|"Status" -> "FAIL", "Reason" -> "DiagDataMissing"|>]];
    omegas = diagData["NormalModeFrequencies"];
    
    (* 7. Get Wavefunction *)
    psi = QED`Model`GetWaveFunction[model, state];
    If[FailureQ[psi], Return[<|"Status" -> "FAIL", "Reason" -> "WaveFunctionError"|>]];

    (* 8. Construct Kinetic Operator Action *)
    (* Replace q_i * q_j -> -hbar^2 * D[psi, phi_i, phi_j] *)
    
    hPsi = Expand[kineticNumeric] /. {
        Times[x___, Subscript[QED`$ChargeSymbol, i_], Subscript[QED`$ChargeSymbol, j_], y___] :> 
            x * (-hbarValue^2 * D[psi, Subscript[QED`$FluxSymbol, i], Subscript[QED`$FluxSymbol, j]]) * y,
        Power[Subscript[QED`$ChargeSymbol, i_], 2] :> 
            (-hbarValue^2 * D[psi, {Subscript[QED`$FluxSymbol, i], 2}])
    };
    
    (* Add Potential Energy part *)
    hPsi = Simplify[hPsi + potentialNumeric * psi 
          /.{Subscript[QED`$FluxSymbol, i_] :> 
          QED`$Phi0Value*Subscript[QED`$FluxSymbol, i]}] // Chop // Simplify;

    (* 9. Calculate Expected Energy *)
    (* E_harm = U_harm(min) + sum(hbar * omega * (n + 1/2)) *)
    (* Check for constant term in potentialNumeric by setting all phi variables to their min values *)
    (* Wait, H_harm is expanded around phi_min. If we set phi -> phi_min_val, we should get the constant term *)
    
    phiToMinVal = Table[
       Subscript[QED`$FluxSymbol, n] -> (Subscript[QED`$FluxSymbol, "min", n] /. eqFluxes),
       {n, nodes}
    ];
    
    constTerm = potentialNumeric /. phiToMinVal;
    
    energyVal = constTerm + Total[(state + 0.5) * omegas * hbarValue];
    ePsi = Simplify[energyVal * psi /.{Subscript[QED`$FluxSymbol, i_] :> QED`$Phi0Value*Subscript[QED`$FluxSymbol, i]}] // Chop;

    (* 10. Residual *)
    residual = Chop[Simplify[hPsi - ePsi]];

    (* 11. Verify Normalization *)
    Module[{phiMinVals, phi0 = QED`$Phi0Value, range, tempVars, psiRescaled, jacobian},
        
        (* 1. Extract numeric equilibrium values *)
        phiMinVals = Values[Flatten[{eqFluxes}]]; (* Flatten handles single rule case *)

        (* 2. Define dimensionless variables xi (order of 1) *)
        tempVars = Table[Unique["xi"], {Length[fluxVars]}];
        
        (* 3. Substitute phi -> phi_min + xi * Phi0 into psi *)
        (* Also ensure psi itself is numeric (substitute L, C, etc.) *)
        psiRescaled = psi /. subRules /. eqFluxes /. Thread[
            fluxVars -> (phiMinVals + tempVars * phi0)
        ];
        
        (* 4. Jacobian for d(phi) -> d(xi): Phi0^D *)
        jacobian = phi0^Length[fluxVars];
        
        (* 5. Integrate over [-8, 8] - comfortable range for NIntegrate *)
        normVal = NIntegrate[
           Abs[psiRescaled]^2, 
           Evaluate[Sequence @@ Table[{xi, -8., 8.}, {xi, tempVars}]],
           Method -> "GlobalAdaptive", (* Fast for smooth functions *)
           MaxRecursion -> 3
        ] * jacobian;
    ];


    <|
      "State" -> state,
      "Status" -> If[PossibleZeroQ[residual], "OK", "CheckResidual"],
      "TotalEnergy" -> energyVal,
      "ResidualExpression" -> residual,
      "H_psi" -> hPsi,
      "E_psi" -> ePsi,
      "Norm" -> normVal
    |>
  ]
];

(* ════════════════════════════════════════════════════════════════ *)
(* 		VERIFICATION                                                *)
(* ════════════════════════════════════════════════════════════════ *)

VerifyDiagonalization[model_Association] := Module[
  {capNum, invLNum, diagData, 
   Nmat, matCDiag, matInvLDiag, 
   expectedCaps, calculatedCaps},
  
  capNum = QED`Model`GetNumericalQuantity[model, "CapacitanceMatrixNumerical"];
  invLNum = QED`Model`GetNumericalQuantity[model, "InductanceMatrixInverseNumerical"];
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];

  If[AnyTrue[{capNum, invLNum, diagData}, # === $Failed || FailureQ[#] &],
    Return[$Failed]
  ];

  If[!AssociationQ[diagData] || !MatrixQ[capNum] || !MatrixQ[invLNum],
    Return[$Failed]
  ];

    Nmat = diagData["FluxTransform"];

    matInvLDiag = Transpose[Nmat] . invLNum . Nmat;
    matCDiag = Transpose[Nmat] . capNum . Nmat;

    expectedCaps = diagData["EffectiveCapacitances"];
    calculatedCaps = Diagonal[matCDiag];

    <|
      "Transformed_L_Inverse" -> Chop[matInvLDiag, 10^-20],
      "Transformed_C" -> Chop[matCDiag, 10^-20],
      "Is_L_Diagonal" -> DiagonalMatrixQ[Chop[matInvLDiag, 10^-10]],
      "Is_C_Diagonal" -> DiagonalMatrixQ[Chop[matCDiag, 10^-10]],
      "EffectiveCapacitances_Check" -> expectedCaps / calculatedCaps
    |>
];

CreateAnnihilationMatrix[dim_Integer] := 
  SparseArray[{i_, j_} /; i == j - 1 -> Sqrt[N[j - 1]], {dim, dim}];

EmbedOperator[op_?MatrixQ, modeIndex_Integer, dims_List] := Module[{ops},
  (* Проверка размерности *)
  If[Dimensions[op] != {dims[[modeIndex]], dims[[modeIndex]]},
     Return[Failure["DimensionMismatch", <|"Message" -> "Operator dimension does not match target mode dimension"|>]]
  ];
  
  (* Создаем список единичных матриц *)
  ops = Table[IdentityMatrix[d, SparseArray], {d, dims}];
  
  (* Подменяем нужную на наш оператор *)
  ops[[modeIndex]] = SparseArray[op];
  
  (* Вычисляем тензорное произведение *)
  Apply[KroneckerProduct, ops]
];

GetBasisOperators[dims_List] := Module[{nModes, singleModeOps, fullOps},
  nModes = Length[dims];
  
  (* Генерируем "маленькие" операторы для каждой моды *)
  singleModeOps = CreateAnnihilationMatrix /@ dims;
  
  (* Расширяем их до полного пространства *)
  fullOps = <|
    "a" -> Table[EmbedOperator[singleModeOps[[k]], k, dims], {k, nModes}],
    "Identity" -> IdentityMatrix[Times @@ dims, SparseArray]
  |>;
  
  (* Добавляем операторы рождения (эрмитово сопряжение) *)
  (* Используем ConjugateTranspose для корректности с комплексными числами, хотя a вещественна *)
  AppendTo[fullOps, "ad" -> (ConjugateTranspose /@ fullOps["a"])];
  
  fullOps
];

ConstructFluxOperators[model_Association, basisOps_Association] := Module[
  {diagData, Nmat, freqs, caps, nModes, hbar, zpf, phiNormalOps, phiLabOps},
  
  (* 1. Извлекаем параметры диагонализации *)
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
  
  (* Если диагонализация еще не выполнена *)
  If[MissingQ[diagData], 
    Return[Failure["MissingDiagonalization", <|"Message" -> "Run CreateCircuitModel and ensure diagonalization is successful first."|>]]
  ];

  Nmat = diagData["FluxTransform"];
  freqs = diagData["NormalModeFrequencies"];
  caps = diagData["EffectiveCapacitances"];
  hbar = QED`$hbarValue; (* Глобальная константа *)
  
  nModes = Length[freqs];
  
  (* Проверка соответствия размерностей *)
  If[Length[basisOps["a"]] != nModes,
     Return[Failure["DimensionMismatch", <|"Message" -> "Number of modes in basisOps does not match model diagonalization."|>]]
  ];

  (* 2. Строим операторы нормальных мод: Phi_k = ZPF_k * (a_k + ad_k) *)
  phiNormalOps = Table[
    With[{
      (* ZPF = Sqrt[hbar / (2 C w)] *)
      (* Добавляем защиту от деления на ноль для 0-й моды, если она есть *)
      coeff = If[TrueQ[freqs[[k]] == 0], 
                0, (* Или обработка для свободного ротатора/заряда, если нужно *)
                Sqrt[hbar / (2 * caps[[k]] * freqs[[k]])]
              ]
      },
      coeff * (basisOps["a"][[k]] + basisOps["ad"][[k]])
    ],
    {k, nModes}
  ];
  
  (* 3. Переходим в лабораторную систему: Phi_lab = N . Phi_normal *)
  (* Nmat - это матрица (Nodes x Modes), phiNormalOps - список матриц (Modes) *)
  (* Dot (.) корректно свернет это в список матриц для узлов *)
  phiLabOps = Nmat . phiNormalOps;
  
  phiLabOps
];

End[];
EndPackage[];