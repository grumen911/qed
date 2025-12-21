BeginPackage["QED`Numeric`", {"QED`Numeric`HarmonicOscillator`"}];

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

ComputeNormalModeFrequencies::usage = "ComputeNormalModeFrequencies[invCap, L] \
computes normal mode frequencies ω_i from eigenvalues of C^(-1)·L matrix. \
Returns frequencies in rad/s (SI units), sorted by increasing frequency.";

PlasmonFrequenciesVsFlux::usage = 
  "PlasmonFrequenciesVsFlux[model] возвращает численную функцию ω[φext_?NumericQ], \
где φext в единицах Φ₀. Возвращает список частот {ω₁, ω₂, ...} в rad/s.";

(* ════════════════════════════════════════════════════════════════ *)
(*                  3D POTENTIAL VISUALIZATION                      *)
(* ════════════════════════════════════════════════════════════════ *)

PlotPotentialSlices3D::usage = 
"PlotPotentialSlices3D[model] creates a 3D visualization of the potential \
energy landscape using slice contour plots through equilibrium points.

Options:
  SliceType -> \"Auto\" | \"CenterPlanes\" | custom
    \"Auto\" (default) - Automatically constructs three orthogonal planes passing \
through characteristic equilibrium points (lowest energy interior points).
    \"CenterPlanes\" - Uses standard coordinate planes (φ₁=0, φ₂=0, φ₃=0).
  
  PlotCenter -> \"GlobalMinimum\" | \"Origin\" | {φ1, φ2, φ3}
    \"GlobalMinimum\" - Centers the plot at the global energy minimum, \
bringing boundary minima into the visible region.
    \"Origin\" (default) - Centers at {0, 0, 0}.
    {φ1, φ2, φ3} - Custom center coordinates (in SI units).
  
  ShowEquilibriumPoints -> True | False
    If True (default), overlays equilibrium points as colored spheres (by energy) \
and red dots for all equilibria.
  
  BoundaryThreshold -> number (default: 0.45)
    Filters out equilibrium points where |φᵢ - center| > threshold × Φ₀. \
Points outside this range are excluded from characteristic point selection.
  
  PlotPoints -> integer (default: 25)
    Resolution of the contour plot. Lower values (15-20) improve performance.
  
  Contours -> integer (default: 15)
    Number of energy contour levels.

Returns:
  Graphics3D object showing potential energy isosurfaces on slice planes, \
with equilibrium points highlighted.

Key features:
• Automatic slice plane calculation through up to 3 lowest-energy interior points
• PlotCenter option for centering at global minimum (reveals boundary minima)
• Energy-based color coding: blue (low) → red (high) via TemperatureMap
• Lazy evaluation via GetNumericalQuantity[model, \"PlotPotentialSlices3D\"]
• Debug mode: Set $DebugPlotPotentialSlices3D = True for diagnostics

Example:
  PlotPotentialSlices3D[$CurrentModel]
  PlotPotentialSlices3D[$CurrentModel, PlotCenter -> \"Origin\"]
  PlotPotentialSlices3D[$CurrentModel, PlotPoints -> 15, Contours -> 10]
";

PlotPotentialSlices3D::noequilibria = "No equilibrium points found. Cannot create visualization.";
PlotPotentialSlices3D::dimension = "Expected 3 flux variables, got `1`. SliceContourPlot3D requires 3D potential.";


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
         constraints, result, minValues, phi0Value, minSymbols},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Потенциальная энергия: U(φ) = H(q=0, φ) *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Константы *)
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
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
  
  
If[$DebugFindEquilibriumPoints === True,
  Print["=== EQUILIBRIUM ANALYSIS ==="];
  
  (* Энергии всех решений *)
  Module[{energies, Emin, Emax, dE},
    energies = Sort[#["Energy"] & /@ solutions];
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
    grouped = GroupBy[solutions, Round[#["Energy"], 10^-25] &];  (* <-- FIX *)
    degeneracies = Sort[Tally[Length /@ Values[grouped]][[All, 1]], Greater];
    Print["Degeneracies: ", Take[degeneracies, UpTo[5]], " solutions per level"];
  ];
  
  (* Топ-3 минимума *)
  Module[{top3},
    top3 = Take[SortBy[solutions, #["Energy"] &], UpTo[3]];
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

(* Опции *)
Options[FindEquilibriumPoints] = {
  GridResolution -> 5,
  MaxResidual -> 10^-5,
  Method -> "Newton"
};

(* Debug флаг *)
$DebugFindEquilibriumPoints = False;


(*
  Physics: Normal mode frequencies from harmonic approximation.
  
  For quadratic Hamiltonian H = (1/2) q^T C^(-1) q + (1/2) φ^T L^(-1) φ,
  normal modes satisfy:
  
  ω_i^2 = eigenvalues(C^(-1) · L^(-1))
  
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


(* ::Section:: *)
(* PlotPotentialSlices3D with PlotCenter option *)

(* ════════════════════════════════════════════════════════════════ *)
(*                          OPTIONS                                 *)
(* ════════════════════════════════════════════════════════════════ *)

Options[PlotPotentialSlices3D] = {
  SliceType -> "Auto",              
  ShowEquilibriumPoints -> True,
  PlotPoints -> 25,
  Contours -> 15,
  BoundaryThreshold -> 0.45,
  PlotCenter -> "Origin"     (* НОВОЕ: "GlobalMinimum" | "Origin" | {φ1, φ2, φ3} *)
};


(* После ComputeNormalModeFrequencies *)

(*
  Physics: Plasmon frequencies as function of external flux.
  
  Returns pure function ω[φext_?NumericQ] where φext is dimensionless (in Φ₀ units).
  For each flux value, performs:
  1. Numerical substitution into C and L⁻¹ matrices
  2. Eigenvalue decomposition of C⁻¹·L⁻¹
  3. Returns sorted frequencies ω_i in rad/s
  
  Reference: Koch et al., PRA 76, 042319 (2007), Eq. 8
*)


PlasmonFrequenciesVsFlux[model_Association] := Module[
  {
    capSym, lindInvSym, hamiltonian, topology, rulesBase, phiExtSym, phi0
  },
  
  (* Аналитические матрицы из модели *)
  capSym     = model["Analytical"]["CapacitanceMatrix"];
  lindInvSym = model["Analytical"]["InductanceMatrix"];  (* L⁻¹ *)
  hamiltonian = model["Analytical"]["Hamiltonian"];
  topology    = model["Topology"];  

  
  (* Физические константы *)
  phiExtSym = QED`$PhiExt;
  phi0      = QED`$Phi0Value;
  
  (* Базовые правила подстановки БЕЗ внешнего потока и φ_min *)
  rulesBase = DeleteCases[
    model["SubstitutionRules"],
    (phiExtSym :> _) | (Subscript[QED`$FluxSymbol, "min", _] :> _)
  ];
  
  (* Возвращаем чисто численную функцию *)
  Function[{phiExtDimensionless},
    Module[{phiExtPhysical, rulesWithFlux, capNum, lindInvNum, 
            invCapNum, omega2, frequencies, equilibriumRules},
      
      If[!NumericQ[phiExtDimensionless],
        Return[$Failed, Module]
      ];

      (* Конвертировать φext из единиц Φ₀ в Weber *)
      phiExtPhysical = phiExtDimensionless * phi0;
      
      (* Подставить текущее значение внешнего потока *)
      rulesWithFlux = Append[rulesBase, phiExtSym -> phiExtPhysical];
      
      equilibriumRules = FindPotentialMinimum[
        hamiltonian,
        topology,
        rulesWithFlux
      ];      

      rulesWithFlux = Join[rulesWithFlux, equilibriumRules];
      
      (* Численные матрицы *)
      capNum     = capSym /. rulesWithFlux;
      lindInvNum = lindInvSym /. rulesWithFlux;
      
      (* C⁻¹ *)
      If[Det[capNum] == 0, Return[$Failed]];
      invCapNum = Inverse[capNum];
      
      (* ω² = eigenvalues(C⁻¹ · L⁻¹) *)
      omega2 = Eigenvalues[N[invCapNum . lindInvNum]];
      
      (* √ω² с сортировкой, комплексные если неустойчивость *)
      frequencies = Sort[Sqrt[omega2 + 0. I], Re[#1] < Re[#2] &];
      
      Chop[frequencies]
    ]
  ]
];



(* ════════════════════════════════════════════════════════════════ *)
(*                     MAIN FUNCTION                                *)
(* ════════════════════════════════════════════════════════════════ *)

PlotPotentialSlices3D[model_Association, opts:OptionsPattern[]] := 
 Module[{topology, hamiltonian, phi0, fluxVars, potential, 
         equilibria, allEnergies, energyMin, energyMax, 
         allFluxCoords, sliceSurf, plot, interiorPoints,
         showPoints, nContours, nPlotPoints, characteristicCoords,
         characteristicPoints, boundaryThreshold, plotCenter, globalMin},
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 1. ИЗВЛЕЧЕНИЕ ПОТЕНЦИАЛА                                         *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  topology = model["Topology"];
  hamiltonian = model["Analytical"]["Hamiltonian"];
  phi0 = QED`$Phi0Value;
  
  (* Независимые потоки *)
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ 
    Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  
  (* Проверка размерности *)
  If[Length[fluxVars] != 3,
    Message[PlotPotentialSlices3D::dimension, Length[fluxVars]];
    Return[$Failed]
  ];
  
  (* Потенциальная энергия U(φ) = H(q=0, φ) с подстановкой параметров *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  potential = potential /. model["SubstitutionRules"];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 2. ПОЛУЧЕНИЕ РАВНОВЕСНЫХ ТОЧЕК  *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  equilibria = GetCacheEntry[
    model["Numerical"]["Cache"]["EquilibriumPoints"],
    model
  ];
  
  If[equilibria === $Failed || Length[equilibria["Solutions"]] == 0,
    Message[PlotPotentialSlices3D::noequilibria];
    Return[$Failed]
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 3. ВЫЧИСЛЕНИЕ ГЛОБАЛЬНОГО ДИАПАЗОНА ЭНЕРГИЙ                      *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  allEnergies = #["Energy"] & /@ equilibria["Solutions"];
  energyMin = Min[allEnergies];
  energyMax = Max[allEnergies];
  
  If[$DebugPlotPotentialSlices3D === True,
    Print["Energy range: [", ScientificForm[energyMin, 3], ", ", 
          ScientificForm[energyMax, 3], "] J"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 3.5. ОПРЕДЕЛЕНИЕ ЦЕНТРА КООРДИНАТ                               *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  plotCenter = Switch[OptionValue[PlotCenter],
    "GlobalMinimum",
    (* Глобальный минимум (наименьшая энергия среди ВСЕХ точек) *)
    globalMin = First[SortBy[equilibria["Solutions"], #["Energy"] &]];
    Values[globalMin["Fluxes"]],
    
    "Origin",
    {0, 0, 0},
    
    _List,
    (* Пользовательские координаты в единицах СИ *)
    OptionValue[PlotCenter],
    
    _,
    (* Fallback *)
    {0, 0, 0}
  ];

  If[$DebugPlotPotentialSlices3D === True,
    Print["PlotCenter option: ", OptionValue[PlotCenter]];
    Print["Plot center (Φ₀ units): ", Round[plotCenter / phi0, 0.001]];
    Print["Plot center (absolute): ", ScientificForm[#, 3]& /@ plotCenter];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 4. ИЗВЛЕЧЕНИЕ КООРДИНАТ ДЛЯ СРЕЗОВ (СДВИНУТЫХ)                  *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Фильтр: Убрать точки на границах ПОСЛЕ сдвига *)
  boundaryThreshold = OptionValue[BoundaryThreshold] * phi0;
  
  interiorPoints = Select[equilibria["Solutions"],
    Module[{coords = Values[#["Fluxes"]] - plotCenter},  (* СДВИГ *)
      AllTrue[Abs[coords], # < boundaryThreshold &]
    ] &
  ];
  
  If[$DebugPlotPotentialSlices3D === True,
    Print["Total equilibrium points: ", Length[equilibria["Solutions"]]];
    Print["Interior points (|φᵢ - center| < ", OptionValue[BoundaryThreshold], "Φ₀): ", 
          Length[interiorPoints]];
    Print["Boundary points removed: ", 
          Length[equilibria["Solutions"]] - Length[interiorPoints]];
  ];
  
  (* Взять до 3 точек с минимальной энергией *)
  characteristicPoints = Take[
    SortBy[interiorPoints, #["Energy"] &], 
    UpTo[3]
  ];

  (* Координаты характерных точек СДВИНУТЫЕ *)
  characteristicCoords = (#["Fluxes"][[All, 2]] - plotCenter) & /@ characteristicPoints;
  
  (* Все координаты равновесных точек для overlay СДВИНУТЫЕ *)
  allFluxCoords = (#["Fluxes"][[All, 2]] - plotCenter) & /@ equilibria["Solutions"];
  
  If[$DebugPlotPotentialSlices3D === True,
    Print["Selected ", Length[characteristicPoints], " characteristic points"];
    Print["Energies: ", ScientificForm[#, 3] & /@ (#["Energy"] & /@ characteristicPoints)];
    Print["Shifted characteristic coords (Φ₀ units):"];
    Do[
      Print["  Point ", i, ": ", Round[characteristicCoords[[i]] / phi0, 0.001]],
      {i, Length[characteristicCoords]}
    ];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 5. ПОСТРОЕНИЕ СРЕЗОВ (3 ортогональные плоскости)                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Module[{min1, min2, min3, v1, v2, n1, n2, n3, d1, d2, d3, coords,
          normals, offsets},
    
    coords = characteristicCoords;  (* УЖЕ СДВИНУТЫЕ *)
    
    {normals, offsets} = Which[
      (* СЛУЧАЙ 1: Есть 3+ точки *)
      Length[coords] >= 3,
      {min1, min2, min3} = coords[[1 ;; 3]];
      
    (* Векторы в плоскости *)
    v1 = min2 - min1;
    v2 = min3 - min1;
    
    (* Нормаль к плоскости P₁: n₁ = v₁ × v₂ *)
    n1 = Cross[v1, v2];
    
    (* DEBUG *)
    If[$DebugPlotPotentialSlices3D === True,
      Print["v1 = ", v1];
      Print["v2 = ", v2];
      Print["n1 = Cross[v1, v2] = ", n1];
      Print["Norm[n1] = ", Norm[n1]];
      Print["Relative threshold: ", Norm[n1] / (Norm[v1] * Norm[v2])];
    ];
    
    (* Относительная проверка коллинеарности *)
    If[Norm[n1] < 10^-6 * Norm[v1] * Norm[v2],
      (* Коллинеарны → fallback *)
      If[$DebugPlotPotentialSlices3D === True,
        Print["Warning: 3 points are collinear. Using phi3 axis for 3rd plane."];
      ];
      
      (* Направление прямой: от min1 к min3 *)
      v1 = min3 - min1;
      
      (* P₁: перпендикулярна v1 и оси z *)
      n1 = Cross[v1, {0, 0, 1}];
      If[Norm[n1] < 10^-10, n1 = Cross[v1, {0, 1, 0}]];
      n1 = n1 / Norm[n1];
      
      (* P₂: перпендикулярна P₁ и v1 *)
      n2 = Cross[n1, v1];
      n2 = n2 / Norm[n2];
      
      (* P₃: вдоль оси phi3 *)
      n3 = {0, 0, 1},
      
      (* НЕ коллинеарны — стандартная логика *)
      n1 = n1 / Norm[n1];
      
      (* P₂: перпендикулярна P₁, проходит через min1-min2 *)
      n2 = Cross[n1, v1];
      n2 = n2 / Norm[n2];
      
      (* P₃: перпендикулярна P₁, проходит через min1-min3 *)
      Module[{v3},
        v3 = min3 - min1;
        n3 = Cross[n1, v3];
        n3 = n3 / Norm[n3];
      ];
    ];
    
    d1 = n1.min1;
    d2 = n2.min1;
    d3 = n3.min1;

    (* DEBUG: проверить, что все плоскости проходят через нужные точки *)
    If[$DebugPlotPotentialSlices3D === True,
      Print["=== PLANE VALIDATION ==="];
      Print["P₁ distances:"];
      Print["  dist(min1, P₁) = ", Abs[n1.min1 - d1]];
      Print["  dist(min2, P₁) = ", Abs[n1.min2 - d1]];
      Print["  dist(min3, P₁) = ", Abs[n1.min3 - d1]];
      Print[""];
      Print["P₂ distances:"];
      Print["  dist(min1, P₂) = ", Abs[n2.min1 - d2]];
      Print["  dist(min2, P₂) = ", Abs[n2.min2 - d2]];
      Print["  dist(min3, P₂) = ", Abs[n2.min3 - d2]];
      Print[""];
      Print["P₃ distances:"];
      Print["  dist(min1, P₃) = ", Abs[n3.min1 - d3]];
      Print["  dist(min2, P₃) = ", Abs[n3.min2 - d3]];
      Print["  dist(min3, P₃) = ", Abs[n3.min3 - d3]];
    ];
      
      {{n1, n2, n3}, {d1, d2, d3}},
      
      (* СЛУЧАЙ 2: Есть 2 точки *)
      Length[coords] == 2,
      {min1, min2} = coords;
      v1 = min2 - min1;
      
      n1 = Cross[v1, {0, 0, 1}];
      If[Norm[n1] < 10^-10, n1 = Cross[v1, {0, 1, 0}]];
      n1 = n1 / Norm[n1];
      
      n2 = Cross[n1, v1];
      n2 = n2 / Norm[n2];
      
      n3 = {0, 0, 1};
      
      d1 = n1.min1;
      d2 = n2.min1;
      d3 = n3.min1;
      
      If[$DebugPlotPotentialSlices3D === True,
        Print["Warning: Only 2 minima found. Using phi3 axis for 3rd plane."];
      ];
      
      {{n1, n2, n3}, {d1, d2, d3}},
      
      (* СЛУЧАЙ 3: < 2 точки → fallback *)
      True,
      If[$DebugPlotPotentialSlices3D === True,
        Print["Warning: Less than 2 minima. Using CenterPlanes fallback."];
      ];
      sliceSurf = "CenterPlanes";
      {{}, {}}
    ];
    
    (* Сохранить для использования в секции 6 *)
    sliceSurf = If[sliceSurf === "CenterPlanes",
      "CenterPlanes",
      {normals, offsets}
    ];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 6. ПОСТРОЕНИЕ SliceContourPlot3D                                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Module[{phi1, phi2, phi3, potentialPlot, coordsPlot, finalSliceSurf},
    
    (* ПОДСТАНОВКА С УЧЁТОМ СДВИГА: U(φ + center) *)
    potentialPlot = potential /. Thread[
      fluxVars -> {phi1 + plotCenter[[1]], 
                   phi2 + plotCenter[[2]], 
                   phi3 + plotCenter[[3]]}
    ];
    
    coordsPlot = allFluxCoords;  (* УЖЕ СДВИНУТЫЕ *)
    
    showPoints = OptionValue[ShowEquilibriumPoints];
    nContours = OptionValue[Contours];
    nPlotPoints = OptionValue[PlotPoints];
    
    (* Построить уравнения плоскостей *)
    finalSliceSurf = Switch[OptionValue[SliceType],
      "CenterPlanes",
      "CenterPlanes",
      
      "Auto" | "ThroughMinima",
      If[sliceSurf === "CenterPlanes",
        "CenterPlanes",
        Module[{normals, offsets, n1, n2, n3, d1, d2, d3, plane1, plane2, plane3},
          {normals, offsets} = sliceSurf;
          {n1, n2, n3} = normals;
          {d1, d2, d3} = offsets;
          
          plane1 = n1[[1]]*phi1 + n1[[2]]*phi2 + n1[[3]]*phi3 == d1;
          plane2 = n2[[1]]*phi1 + n2[[2]]*phi2 + n2[[3]]*phi3 == d2;
          plane3 = n3[[1]]*phi1 + n3[[2]]*phi2 + n3[[3]]*phi3 == d3;
          
          If[$DebugPlotPotentialSlices3D === True,
            Print["Slice planes (through minima):"];
            Print["  P₁: ", plane1];
            Print["  P₂: ", plane2];
            Print["  P₃: ", plane3];
          ];

          {plane1, plane2, plane3}
        ]
      ],
      
      _,
      OptionValue[SliceType]
    ];
    
    plot = SliceContourPlot3D[
      potentialPlot,
      finalSliceSurf,
      {phi1, -0.5*phi0, 0.5*phi0},
      {phi2, -0.5*phi0, 0.5*phi0},
      {phi3, -0.5*phi0, 0.5*phi0},
      
      ColorFunctionScaling -> False,
      ColorFunction -> Function[z,
        ColorData["TemperatureMap"][
          Rescale[z, {energyMin, energyMax}, {0, 1}]
        ]
      ],
      
      PlotRange -> {energyMin, energyMax},
      Contours -> nContours,
      PlotLegends -> Automatic,
      PlotPoints -> nPlotPoints,
      
      AxesLabel -> {
        Subscript[QED`$FluxSymbol, 1],
        Subscript[QED`$FluxSymbol, 2],
        Subscript[QED`$FluxSymbol, 3]
      },
      
      BoxRatios -> {1, 1, 1},
      ImageSize -> 600,
      
      PlotLabel -> Style[
        "Potential Energy (center: " <> 
        Replace[OptionValue[PlotCenter], 
          {"GlobalMinimum" -> "global min", "Origin" -> "origin", _ -> "custom"}] <> ")",
        12
      ]
    ];
    
    (* Overlay точек *)
    If[showPoints,
      Module[{sphereRadius, colors},
        
        sphereRadius = 0.05 * phi0;
        
        colors = ColorData["Rainbow"] /@ Rescale[
          #["Energy"] & /@ characteristicPoints,
          {energyMin, energyMax},
          {0, 1}
        ];
        
        Show[
          plot,
          Graphics3D[{
            PointSize[0.02],
            Red,
            Point /@ coordsPlot,
            
            Opacity[0.8],
            MapThread[{#1, Sphere[#2, sphereRadius]} &, {colors, characteristicCoords}]
          }]
        ]
      ],
      plot
    ]
  ]
];


(* ════════════════════════════════════════════════════════════════ *)
(*                       DEBUG FLAG                                 *)
(* ════════════════════════════════════════════════════════════════ *)

$DebugPlotPotentialSlices3D = False;
  

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


End[];
EndPackage[];