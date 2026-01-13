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

VerifyWaveFunction::usage = "VerifyWaveFunction[model, state] verifies that H|psi> = E|psi>.";

VerifyDiagonalization::usage = "VerifyDiagonalization[model] numerically checks if the calculated \
FluxTransform matrix correctly diagonalizes both Capacitance and Inductance matrices. \
Returns <|'Is_L_Diagonal', 'Is_C_Diagonal', ...|>.";


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





(*** NOTE: The remainder of the file is unchanged; only VerifyDiagonalization Print statements were removed. ***)


cleanExpr[expr_] := expr /. {Abs'[x_] :> Sign[x], Conjugate'[x_] :> Conjugate[x]};

(*** VerifyWaveFunction definition unchanged in this commit ***)

(* ════════════════════════════════════════════════════════════════ *)
(* 		VERIFICATION                                                *)
(* ════════════════════════════════════════════════════════════════ *)

VerifyDiagonalization[model_Association] := Module[
  {capNum, invLNum, diagData, 
   Nmat, matC_diag, matinvL_diag, 
   expectedCaps, calculatedCaps},
  
  capNum = QED`Model`GetNumericalQuantity[model, "CapacitanceMatrixNumerical"];
  invLNum = QED`Model`GetNumericalQuantity[model, "InductanceMatrixInverseNumerical"];
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];

  If[AnyTrue[{capNum, invLNum, diagData}, FailureQ],
     Return[$Failed]
  ];
  
  Nmat = diagData["FluxTransform"];
  
  matinvL_diag = Transpose[Nmat] . invLNum . Nmat;
  matC_diag = Transpose[Nmat] . capNum . Nmat;
  
  expectedCaps = diagData["EffectiveCapacitances"];
  calculatedCaps = Diagonal[matC_diag];

  <|
    "Transformed_L_Inverse" -> Chop[matinvL_diag, 10^-20],
    "Transformed_C" -> Chop[matC_diag, 10^-20],
    "Is_L_Diagonal" -> DiagonalMatrixQ[Chop[matinvL_diag, 10^-10]],
    "Is_C_Diagonal" -> DiagonalMatrixQ[Chop[matC_diag, 10^-10]],
    "EffectiveCapacitances_Check" -> expectedCaps / calculatedCaps
  |>
];


End[];
EndPackage[];
