BeginPackage["QED`Numeric`"];

Needs["QED`Model`"];


FindPotentialMinimum::usage = "FindPotentialMinimum[hamiltonian, topology, substitutionRules] \
numerically finds equilibrium flux values φ_min that minimize potential energy U(φ). \
Uses PrincipalAxis method (gradient-free local optimization) starting from external flux φ_ext. \
Optimized for smooth potentials with good initial guess (~0.002 sec). \
Returns substitution rules: {φ₁ -> value₁, φ₂ -> value₂, ...} in Weber.";

FindEquilibriumPoints::usage = "FindEquilibriumPoints[hamiltonian, topology, substitutionRules] \
finds all equilibrium flux configurations by solving ∇U = 0 on a grid of starting points. \
Returns Association with list of solutions, energies, and residuals.";

FindPotentialMinimumContinuation::badstep = 
  "Continuation failed at step `1` of `2`. Try reducing StepSize option.";

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

BuildNumericalHamiltonian::usage = 
"BuildNumericalHamiltonian[model, basisOps, fluxOps] constructs the full Hamiltonian matrix.
It adds the harmonic part (sum hbar*w*ad*a) and the non-linear Josephson terms.
Subtracts the quadratic part of the cosine potential to avoid double-counting (since it's included in H_harm).";

GetLabMatrixElements::usage = "GetLabMatrixElements[model] computes the transition matrix elements \
(<0|Phi_lab|1_k>, <0|Q_lab|1_k>) for each node and each normal mode k. \
Returns an Association: <| \"Flux\" -> <| node -> <| mode -> val |> |>, ... |>.";

CalculateFermiRates::usage = "CalculateFermiRates[model, options] calculates relaxation rates (T1). \
Channels included: \n\
1. Inductive Coupling (RL): Relaxation via mutual inductance M to a resistor R (Flux bias line). \n\
2. Capacitive Coupling (RC): Relaxation via capacitor Cc to a resistor R (Readout line / Purcell).";

CalculateDephasingRates::usage = "CalculateDephasingRates[model, opts] calculates pure dephasing time T_phi \
using robust numerical differentiation (Central Finite Difference) via FindPotentialMinimumContinuation. \
Returns Association with derivatives dOmega/dPhi, d2Omega/dPhi2 and estimated rates.";

GenerateSweepPipeline::usage = "GenerateSweepPipeline[modelAssoc, targetQuantity, opts] returns a function f[phiExt] that computes \
targetQuantity at a given external flux φ_ext. \n\nArguments:\n\
  modelAssoc: Association containing the QED model and pre-compiled engines.\n\
  targetQuantity: String specifying the quantity to compute ('EquilibriumFluxes', 'SystemMatrices', 'PlasmonFrequencies').\n\
  opts: Options for the continuation method (e.g., StepSize).\n\nExample:\n\
  sweepFunc = GenerateSweepPipeline[modelAssoc, 'PlasmonFrequencies'];\n\
  frequencies = sweepFunc[0.25];";


Begin["`Private`"];


Options[GenerateSweepPipeline] = {
  MaxFluxStep -> 0.05
};

Options[CalculateDephasingRates] = {
  "FluxStep" -> 1.0*^-4,           (* Шаг h в единицах Phi0 *)
  "FluxNoiseAmplitude" -> 1.0*^-6, (* A_Phi в единицах Phi0 *)
  "PinkNoiseLogFactor" -> 3.0      (* Sqrt[2 ln(omega * t)] *)
};

Options[CalculateFermiRates] = {
  (* Inductive Channel parameters *)
  "MutualInductance" -> 2.0*^-12, (* M ~ 2 pH *)
  "InductiveLineResistance" -> 50.0,
  
  (* Capacitive Channel parameters *)
  "CouplingCapacitance" -> 1.0*^-15, (* Cc ~ 1 fF *)
  "CapacitiveLineResistance" -> 50.0,
  
  "PortNode" -> 1 (* Node connected to readout/control *)
};

Options[FindEquilibriumPoints] = {
  GridResolution -> 5,
  MaxResidual -> 10^-5,
  Method -> "Newton"
};

$DebugFindPotentialMinimum = False;
$DebugFindEquilibriumPoints = False;


GenerateSweepPipeline[modelAssoc_, targetQuantity_String, OptionsPattern[]] := Module[
  {
    engines, staticMats, invCMatrix, paramVector, maxStep,
    fastGrad, fastHess, fastLInv, numVars,
    pathHistory = <||>,
    paramSymbols, phiExtIndex
  },
  
  engines = QED`Model`GetNumericalQuantity[modelAssoc, "CompiledEngines"];
  If[engines === $Failed, Return[$Failed]];
  
  staticMats = QED`Model`GetNumericalQuantity[modelAssoc, "StaticMatrices"];
  invCMatrix = staticMats[[2]];
  
  paramVector = QED`Model`GetParameterVector[modelAssoc];
  
  (* --- УМНЫЙ РОУТИНГ ПАРАМЕТРА ПОТОКА --- *)
  paramSymbols = QED`Model`GetParameterSymbols[modelAssoc];
  phiExtIndex = FirstPosition[paramSymbols, QED`$PhiExt];
  If[MissingQ[phiExtIndex], Return[$Failed]];
  phiExtIndex = First[phiExtIndex];
  
  numVars = Length[Cases[modelAssoc["Topology"]["Nodes"], Except[modelAssoc["Topology"]["GroundNode"]]]];
  {fastGrad, fastHess, fastLInv} = engines;
  maxStep = OptionValue[MaxFluxStep];

  (* ВОЗВРАЩАЕМОЕ ЗАМЫКАНИЕ *)
  Function[{phiExtReq},
    Module[
      {targetPhi = phiExtReq, nearestPhi, currentGuess, steps, stepSize, currentPhi, i, currentParamVector},
      
      currentParamVector = paramVector; (* Создаем локальную копию для мутаций *)
      
      If[Length[pathHistory] === 0,
        nearestPhi = 0.; 
        currentGuess = ConstantArray[0., numVars];
        ,
        nearestPhi = First[Nearest[Keys[pathHistory], targetPhi]];
        currentGuess = pathHistory[nearestPhi];
      ];

      If[Abs[nearestPhi - targetPhi] > 10^-8 || Length[pathHistory] === 0,
        steps = Ceiling[Abs[targetPhi - nearestPhi] / maxStep];
        If[steps == 0, steps = 1]; 
        
        stepSize = (targetPhi - nearestPhi) / steps;
        
        Do[
          currentPhi = nearestPhi + i * stepSize;
          
          (* МУТИРУЕМ внешний поток в векторе параметров (переводим в Веберы!) *)
          currentParamVector[[phiExtIndex]] = currentPhi * QED`$Phi0Value;
          
          (* Передаем только 3 аргумента! *)
          currentGuess = QED`Numeric`Calculators`CalcEquilibrium[
            engines, currentGuess, currentParamVector
          ];
          
          pathHistory[currentPhi] = currentGuess;
          ,
          {i, 1, steps}
        ];
      ];

      (* Финальное значение потока для конвейера матриц *)
      currentParamVector[[phiExtIndex]] = targetPhi * QED`$Phi0Value;

      Switch[targetQuantity,
        "EquilibriumFluxes", 
          currentGuess,
          
        "SystemMatrices",    
          QED`Numeric`Calculators`CalcSystemMatrices[fastLInv, currentGuess, currentParamVector],
          
        "HarmonicDiagonalization",
          QED`Numeric`Calculators`CalcHarmonicDiagonalization[
            invCMatrix, 
            QED`Numeric`Calculators`CalcSystemMatrices[fastLInv, currentGuess, currentParamVector]["InverseInductance"]
          ],

        "PlasmonFrequencies", 
          QED`Numeric`Calculators`CalcHarmonicDiagonalization[
            invCMatrix, 
            QED`Numeric`Calculators`CalcSystemMatrices[fastLInv, currentGuess, currentParamVector]["InverseInductance"]
          ]["NormalModeFrequencies"],
        
        "SMatrix_1_2",
          Module[{invLNum, portIndices, scatData},
            scatData = modelAssoc["Analytical"]["Scattering"]["1_2"];
            If[scatData === $Failed, Return[$Failed]];
            
            invLNum = QED`Numeric`Calculators`CalcSystemMatrices[fastLInv, currentGuess, currentParamVector]["InverseInductance"];
            portIndices = scatData["PortIndices"];
            
            Function[{omegaReq},
              QED`Numeric`Calculators`CalcSMatrixNumeric[omegaReq, staticMats[[1]], invLNum, portIndices, 50.0]
            ]
          ],
          
        "SMatrix_1_4",
          Module[{invLNum, portIndices, scatData},
            scatData = modelAssoc["Analytical"]["Scattering"]["1_4"];
            If[scatData === $Failed, Return[$Failed]];
            
            invLNum = QED`Numeric`Calculators`CalcSystemMatrices[fastLInv, currentGuess, currentParamVector]["InverseInductance"];
            portIndices = scatData["PortIndices"];
            
            Function[{omegaReq},
              QED`Numeric`Calculators`CalcSMatrixNumeric[omegaReq, staticMats[[1]], invLNum, portIndices, 50.0]
            ]
          ],
        _, 
          $Failed
      ]
    ]
  ]
];


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

BuildNumericalHamiltonian::usage = "BuildNumericalHamiltonian[model, basisOps, fluxOps] constructs the diagonal harmonic Hamiltonian H0. Returns a Dense Matrix to ensure stable Eigensystem sorting.";

BuildNumericalHamiltonian[model_Association, basisOps_Association, fluxOps_List] := Module[
  {freqs, hbar, H0, numModes, diagData, denseH0},

  (* 1. Загрузка данных (сохраняем, чтобы прогреть кэш модели) *)
  (* Возможно, HarmonicDiagonalization ленивая и требует этих вызовов *)
  QED`Model`GetNumericalQuantity[model, "HamiltonianNumerical"];
  QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
  
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];

  If[MissingQ[diagData] || FailureQ[diagData], 
     Return[Failure["MissingData", <|"Message" -> "Diagonalization failed."|>]]
  ];

  freqs = diagData["NormalModeFrequencies"];
  
  If[MissingQ[freqs], 
     Return[Failure["MissingFreqs", <|"Message" -> "Frequencies missing."|>]]
  ];

  (* 2. Строим разреженный H0 *)
  hbar = QED`$hbarValue;
  numModes = Length[freqs];

  H0 = Sum[
    hbar * freqs[[k]] * (basisOps["ad"][[k]] . basisOps["a"][[k]]),
    {k, numModes}
  ];

  (* 3. МАГИЯ ЗДЕСЬ: Принудительная конвертация в Dense + Numeric *)
  (* Это эмулирует то, что раньше делал MatrixExp *)
  (* Normal[] превращает SparseArray в обычный список списков *)
  (* N[] гарантирует, что числа вещественные (MachinePrecision), а не точные дроби *)
  denseH0 = N[Normal[H0]];

  (* Возвращаем Re, чтобы убрать мнимый шум порядка 10^-18 *)
  Re[denseH0]
];

GetLabMatrixElements[model_Association] := Module[
  {diagData, Nmat, Mmat, freqs, caps, hbar, numModes, numNodes, 
   fluxZPFCoeffs, chargeZPFCoeffs, fluxElements, chargeElements},

  (* 1. Fetch Diagonalization Data *)
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
  
  If[MissingQ[diagData] || FailureQ[diagData], 
     Return[Failure["MissingData", <|"Message" -> "HarmonicDiagonalization not found."|>]]
  ];

  Nmat = diagData["FluxTransform"];   (* shape: [NumNodes, NumModes] *)
  Mmat = diagData["ChargeTransform"]; (* shape: [NumNodes, NumModes] *)
  freqs = diagData["NormalModeFrequencies"];
  caps = diagData["EffectiveCapacitances"];
  hbar = QED`$hbarValue;

  {numNodes, numModes} = Dimensions[Nmat];

  (* 2. Calculate Normalization Scalars (ZPF of the mode itself) *)
  (* phi_zpf_k = Sqrt[hbar / (2 * C_k * w_k)] *)
  fluxZPFCoeffs = Table[
    If[TrueQ[freqs[[k]] == 0], 0., Sqrt[hbar / (2.0 * caps[[k]] * freqs[[k]])]], 
    {k, numModes}
  ];
  
  (* q_zpf_k = Sqrt[(hbar * C_k * w_k) / 2] *)
  chargeZPFCoeffs = Table[
    If[TrueQ[freqs[[k]] == 0], 0., Sqrt[(hbar * caps[[k]] * freqs[[k]]) / 2.0]], 
    {k, numModes}
  ];

  (* 3. Compute Matrix Elements (ZPF projected to nodes) *)
  (* Using Association to prevent Part::partw errors during lookup *)
  
  (* Flux Elements: <0 | Phi_node_i | 1_mode_k> *)
  fluxElements = Association @ Table[
    i -> Association @ Table[
       k -> Nmat[[i, k]] * fluxZPFCoeffs[[k]], 
       {k, numModes}
    ],
    {i, numNodes}
  ];

  (* Charge Elements: <0 | Q_node_i | 1_mode_k> *)
  (* Note: The operator is i(a^dagger - a), so element <0|Q|1> is -i * coeff *)
  chargeElements = Association @ Table[
    i -> Association @ Table[
       k -> -I * Mmat[[i, k]] * chargeZPFCoeffs[[k]], 
       {k, numModes}
    ],
    {i, numNodes}
  ];

  <|
    "Flux" -> fluxElements,
    "Charge" -> chargeElements,
    "Units" -> <|"Flux" -> "Wb", "Charge" -> "C"|>
  |>
];

CalculateFermiRates[model_Association, opts:OptionsPattern[]] := Module[
  {
    elements, diagData, freqs, 
    currentOpNum, fluxVars, currentGradient, 
    voltageOpsNum, voltageOp, chargeVars, voltageGradient, (* NEW *)
    hbar, numModes, numNodes, nodes,
    mInd, rInd, cCap, rCap, portNode,
    inductiveRates, capacitiveRates, w, 
    totalT1
  },

  (* 1. PREPARE DATA *)
  elements = GetLabMatrixElements[model];
  If[FailureQ[elements], Return[$Failed]];
  
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
  If[FailureQ[diagData], Return[$Failed]];
  
  freqs = diagData["NormalModeFrequencies"];
  (* effCaps больше не нужны явно, они "зашиты" в оператор напряжения *)
  
  hbar = QED`$hbarValue;
  numModes = Length[freqs];
  
  nodes = Cases[model["Topology"]["Nodes"], Except[model["Topology"]["GroundNode"]]];
  numNodes = Length[nodes];
  
  (* Variables for differentiation *)
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  chargeVars = Subscript[QED`$ChargeSymbol, #] & /@ nodes; (* NEW *)

  (* --- LOAD OPERATORS --- *)
  
  (* A. Current Operator *)
  currentOpNum = QED`Model`GetNumericalQuantity[model, "CurrentOperatorNumerical"];
  If[FailureQ[currentOpNum], currentOpNum = 0];
  currentGradient = D[currentOpNum, {fluxVars}]; 
  
  (* B. Voltage Operators [NEW] *)
  voltageOpsNum = QED`Model`GetNumericalQuantity[model, "VoltageOperatorsNumerical"];
  If[FailureQ[voltageOpsNum], voltageOpsNum = <||>];
  
  (* Extract parameters *)
  portNode = OptionValue["PortNode"]; 
  (* Fallback to first node if portNode is not in keys *)
  voltageOp = If[KeyExistsQ[voltageOpsNum, portNode], 
      voltageOpsNum[portNode], 
      (* Fallback: try to construct naive V = q/C approx or just 0 *)
      0 
  ];
  
  (* Compute weights w_i = dV/dq_i (row of inverse capacitance matrix) *)
  voltageGradient = D[voltageOp, {chargeVars}];

  mInd = OptionValue["MutualInductance"];
  rInd = OptionValue["InductiveLineResistance"];
  cCap = OptionValue["CouplingCapacitance"];
  rCap = OptionValue["CapacitiveLineResistance"];


  (* 2. CALCULATE RATES *)

  (* A. Inductive Channel (Flux Noise)
     Interaction: H_int = M * I_circ * I_bias
     Rate: Gamma = (2 * w * M^2 / (hbar * R)) * |<0|I_circ|k>|^2
  *)
  inductiveRates = Table[
    w = freqs[[k]];
    If[TrueQ[w == 0], 0.,
      Module[{iElem, rate},
        (* <0|I|k> = Sum[ dI/dphi_n * <phi_n> ] *)
        iElem = Sum[currentGradient[[n]] * elements["Flux"][nodes[[n]]][k], {n, 1, numNodes}];
        rate = (2.0 * w * mInd^2 * Abs[iElem]^2) / (hbar * rInd);
        rate
      ]
    ],
    {k, numModes}
  ];

  (* Capacitive (Charge Noise) *)
  (* Rate = (2 * R * Cc^2 * w / hbar) * |<0|V_node|k>|^2 *)
  capacitiveRates = Table[
    w = freqs[[k]];
    If[TrueQ[w == 0], 0.,
      Module[{vElem, rate},
        (* <0|V|k> = Sum[ dV/dq_n * <q_n> ] *)
        (* This accounts for the fact that V_node depends on charges on ALL nodes *)
        vElem = Sum[
            voltageGradient[[n]] * elements["Charge"][nodes[[n]]][k], 
            {n, 1, numNodes}
        ];
        
        (* Note: C_eff is absorbed into vElem (since V ~ q/C_eff) *)
        rate = (2.0 * rCap * cCap^2 * w * Abs[vElem]^2) / hbar;
        rate
      ]
    ],
    {k, numModes}
  ];

  (* 3. Total T1 *)
  totalT1 = Table[
     Module[{gammaTot},
       gammaTot = inductiveRates[[k]] + capacitiveRates[[k]];
       If[gammaTot < 1.0*^-20, Infinity, 1.0 / gammaTot]
     ],
     {k, numModes}
  ];

  <|
    "Modes" -> Range[numModes],
    "Frequencies" -> Reverse[freqs], 
    "InductiveRelaxationRate" -> Reverse[inductiveRates],
    "CapacitiveRelaxationRate" -> Reverse[capacitiveRates],
    "TotalT1" -> Reverse[totalT1]
  |>
];

CalculateDephasingRates[model_Association, opts:OptionsPattern[]] := Module[
  {
    h, A, logFac,
    freqFunc, phiExtDimless,
    w0, wPlus, wMinus,
    d1, d2,
    gamma1, gamma2, totalRate, tPhi
  },

  (* 1. Опции *)
  h = OptionValue["FluxStep"];
  A = OptionValue["FluxNoiseAmplitude"];
  logFac = OptionValue["PinkNoiseLogFactor"];
  
  (* 2. Узнаем текущую рабочую точку (Внешний поток из первичных параметров) *)
  phiExtDimless = (QED`$PhiExt /. QED`Model`GetStaticRules[model]) / QED`$Phi0Value;
  If[!NumericQ[phiExtDimless], phiExtDimless = 0.0];

  (* 3. Центральная частота (мгновенно из кэша O(1)) *)
  w0 = QED`Model`GetNumericalQuantity[model, "PlasmonFrequencies"];
  If[FailureQ[w0], Return[$Failed]];

  (* 4. Создаем конвейер ТОЛЬКО для расчета боковых сдвигов *)
  freqFunc = GenerateSweepPipeline[model, "PlasmonFrequencies"];
  If[freqFunc === $Failed, Return[$Failed]];

  wPlus = freqFunc[phiExtDimless + h];
  wMinus = freqFunc[phiExtDimless - h];
  
  If[AnyTrue[{wPlus, wMinus}, FailureQ], Return[$Failed]];

  (* 5. Численные производные (центральная разность) *)
  d1 = (wPlus - wMinus) / (2 * h);
  d2 = (wPlus - 2*w0 + wMinus) / (h^2);
  
  (* 6. Скорости дефазировки *)
  gamma1 = A * logFac * Abs[d1];
  gamma2 = (A^2) * logFac * Abs[d2]; 
  totalRate = Sqrt[gamma1^2 + gamma2^2];

  (* 7. Время T_phi (защита от деления на ноль) *)
  tPhi = Map[
    Function[r, If[TrueQ[r < 1.0*^-20], Infinity, 1.0 / r]],
    totalRate
  ];

  <|
    "Frequencies" -> w0,
    "dOmega_dPhi" -> d1,
    "d2Omega_dPhi2" -> d2,
    "DephasingRate" -> totalRate,
    "DephasingTime" -> tPhi
  |>
];

End[];
EndPackage[];