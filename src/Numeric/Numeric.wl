BeginPackage["QED`Numeric`"];

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

TestOptimizationMethods::usage = "1232";

Begin["`Private`"];

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
         constraints, result, minValues, phi0Value},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Потенциальная энергия: U(φ) = H(q=0, φ) *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Константы *)
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  
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
        "SearchPoints" -> 30,        (* 30 случайных стартовых точек *)
        "RandomSeed" -> 12345,       (* воспроизводимость *)
        "PostProcess" -> {           (* локальная доводка *)
          "FindMinimum",
          Method -> "QuasiNewton"
        }
      },
      MaxIterations -> 100,
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
  
  If[result === $Failed || !NumericQ[result[[1]]],
    (* Fallback: использовать внешний поток как приближение *)
    Print["Warning: Minimization failed. Using φ_min ≈ φ_ext."];
    minValues = Thread[fluxVars -> externalFlux],
    
    (* Успех: конвертировать обратно в Weber *)
    minValues = Thread[fluxVars -> (fluxVars /. result[[2]]) * phi0Value]
  ];
  
  minValues
];

(* Глобальная переменная для отладки *)
$DebugFindPotentialMinimum = False;


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

(* Опции *)
Options[FindEquilibriumPoints] = {
  GridResolution -> 5,
  MaxResidual -> 10^-5,
  Method -> "Newton"
};

(* Debug флаг *)
$DebugFindEquilibriumPoints = True;
  

PrepareNumericModel[symModel_Association, params_Association] := Module[{sol},
  (* подготовка численных функций из символики *)
  sol
];

ComputeEvolution[model_, tmax_?NumericQ] := Module[{sol},
  (* NDSolve *)
  sol
];

End[];
EndPackage[];