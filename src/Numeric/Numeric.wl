BeginPackage["QED`Numeric`", {"QED`Model`"}];

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

(* ════════════════════════════════════════════════════════════════ *)
(*                  3D POTENTIAL VISUALIZATION                      *)
(* ════════════════════════════════════════════════════════════════ *)

PlotPotentialSlices3D::usage = "PlotPotentialSlices3D[model, opts] \
creates 3D slice visualization of potential energy U(φ₁, φ₂, φ₃) using SliceContourPlot3D. \
Automatically computes equilibrium points if not cached and places slices through them. \
Returns Graphics3D object.";

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
    "Frequencies" -> frequencies,
    "IsStable" -> AllTrue[omega2, # > threshold &],
    "NumUnstableModes" -> Count[omega2, x_ /; x < -threshold]
  |>
];


PlotPotentialSlices3D[model_Association, opts:OptionsPattern[]] := 
 Module[{topology, hamiltonian, phi0, fluxVars, potential, 
         equilibria, allEnergies, energyMin, energyMax, 
         allFluxCoords, slicePositions, sliceSurf, plot, 
         showPoints, nContours, nPlotPoints},
  
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
  (* 2. ПОЛУЧЕНИЕ РАВНОВЕСНЫХ ТОЧЕК 									 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  equilibria = GetNumericalQuantity[model, "EquilibriumPoints"];
  
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
  (* 4. ИЗВЛЕЧЕНИЕ КООРДИНАТ ДЛЯ СРЕЗОВ                               *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Взять только характерные точки (с наименьшей энергией) *)
  characteristicPoints = Take[
    SortBy[equilibria["Solutions"], #["Energy"] &], 
    UpTo[5]  (* максимум 5 точек *)
  ];
  
  (* Все координаты равновесных точек для overlay *)
  allFluxCoords = (#["Fluxes"][[All, 2]]) & /@ equilibria["Solutions"];
  
  (* Координаты характерных точек для срезов *)
  characteristicCoords = (#["Fluxes"][[All, 2]]) & /@ characteristicPoints;
  
  (* Уникальные значения по каждой оси для срезов *)
  slicePositions = Union /@ Transpose[characteristicCoords];
  
  If[$DebugPlotPotentialSlices3D === True,
    Print["Selected ", Length[characteristicPoints], " characteristic points"];
    Print["Energies: ", #["Energy"] & /@ characteristicPoints];
    Print["Slice positions:"];
    Print["  φ₁ (", Length[slicePositions[[1]]], " slices): ", slicePositions[[1]]];
    Print["  φ₂ (", Length[slicePositions[[2]]], " slices): ", slicePositions[[2]]];
    Print["  φ₃ (", Length[slicePositions[[3]]], " slices): ", slicePositions[[3]]];
    Print["Total planes: ", Total[Length /@ slicePositions]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 5. ПОСТРОЕНИЕ СРЕЗОВ (пока стандартные, TODO: кастомные через минимумы) *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  sliceSurf = Switch[OptionValue[SliceType],
    "Auto",
    (* Срезы через все найденные равновесные точки *)
    {
      {"XStackedPlanes", slicePositions[[1]]},
      {"YStackedPlanes", slicePositions[[2]]},
      {"ZStackedPlanes", slicePositions[[3]]}
    },
    
    "CenterPlanes",
    "CenterPlanes",
    
    "ThroughMinima",
    (* TODO: Реализовать кастомные плоскости через три минимума *)
    (Print["SliceType -> \"ThroughMinima\" not yet implemented. Using \"CenterPlanes\"."];
     "CenterPlanes"),
    
    _,
    (* Пользовательский surf *)
    OptionValue[SliceType]
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 6. ПОСТРОЕНИЕ SliceContourPlot3D                                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Module[{phi1, phi2, phi3, potentialPlot, coordsPlot},
    
    (* Подстановка Subscript -> простые символы *)
    potentialPlot = potential /. Thread[fluxVars -> {phi1, phi2, phi3}];
    coordsPlot = allFluxCoords /. Thread[fluxVars -> {phi1, phi2, phi3}];
    
    showPoints = OptionValue[ShowEquilibriumPoints];
    nContours = OptionValue[Contours];
    nPlotPoints = OptionValue[PlotPoints];
    
    plot = SliceContourPlot3D[
      potentialPlot,
      sliceSurf,
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
      ImageSize -> 600
    ];
    
    (* Overlay точек *)
    If[showPoints,
      Show[
        plot,
        Graphics3D[{
          PointSize[0.015],
          Red,
          Point /@ coordsPlot
        }]
      ],
      plot
    ]
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 7. OVERLAY РАВНОВЕСНЫХ ТОЧЕК (если опция включена)               *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  If[showPoints,
    Show[
      plot,
      Graphics3D[{
        PointSize[0.015],
        Red,
        Point /@ allFluxCoords
      }]
    ],
    plot
  ]
];


(* ════════════════════════════════════════════════════════════════ *)
(*                          OPTIONS                                 *)
(* ════════════════════════════════════════════════════════════════ *)

Options[PlotPotentialSlices3D] = {
  SliceType -> "Auto",              (* "Auto" | "CenterPlanes" | "ThroughMinima" | custom *)
  ShowEquilibriumPoints -> True,    (* overlay маркеры на равновесных точках *)
  PlotPoints -> 25,                 (* разрешение *)
  Contours -> 15,                   (* количество изолиний *)
  MaxCharacteristicPoints -> 5      (* макс. количество точек для срезов *)
};


(* ════════════════════════════════════════════════════════════════ *)
(*                       DEBUG FLAG                                 *)
(* ════════════════════════════════════════════════════════════════ *)

$DebugPlotPotentialSlices3D = True;
  

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