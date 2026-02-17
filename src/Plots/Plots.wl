BeginPackage["QED`Plots`"];

PlotPlasmonSpectrum::usage = 
  "PlotPlasmonSpectrum[model] строит график зависимости плазмонных частот \
от внешнего магнитного потока Φext.

Options:
  NumModes -> 2 (default) - количество мод для отображения
  FluxRange -> {-0.5, 0.5} - диапазон внешнего потока в единицах Φ₀
  FrequencyUnit -> \"GHz\" | \"MHz\" | \"rad/s\" - единицы частоты

Все стандартные опции Plot также поддерживаются.

Performance:
  Использует PlotPoints -> 25 и MaxRecursion -> 1 для ускорения.
  Каждая точка требует ~0.04 сек (вызов FindPotentialMinimum + eigenvalues).
  Типичное время построения графика: ~2 секунды для 50 точек.

Physics:
  Отображает собственные частоты ω_i(Φext) из гармонического приближения,
  где Φext - внешний магнитный поток через индуктивную петлю.
  
Reference: Koch et al., PRA 76, 042319 (2007), Eq. 8";


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

PlotLabMatrixElements::usage = 
"PlotLabMatrixElements[model] строит таблицу матричных элементов \
(<0|Φ|1> и <0|Q|1>) для лабораторных узлов и нормальных мод.
Отображает вклад каждой моды в колебания на конкретном узле.";

PlotFermiRates::usage = "PlotFermiRates[model] displays a table of relaxation times (T1) \
calculated via Fermi's Golden Rule, separated by noise channel.";

PlotGenericFluxSweep::usage = "PlotGenericFluxSweep[model] plots eigenfrequencies using \
the generic GenerateFluxSweep method. \
Used for verification of the generic sweep architecture.";

PlotRelaxationTime::usage = "PlotRelaxationTime[model] plots the relaxation time T1 \
dependence on external flux using generic sweep and CalculateFermiRates.
Options: Same as PlotPlasmonSpectrum (NumModes, FluxRange).";

PlotDephasingRates::usage = "PlotDephasingRates[model] displays a table of pure dephasing times (T_phi), \
broken down by 1st order (flux slope) and 2nd order (curvature) contributions.";

PlotDephasingTime::usage = "PlotDephasingTime[model] plots the pure dephasing time (T_phi) dependence on external flux.
Options:
  NumModes -> Integer (default 2)
  FluxRange -> {min, max} (default {-0.5, 0.5})
  \"DephasingContribution\" -> \"Total\" | \"FirstOrder\" | \"SecondOrder\"
  \"FluxNoiseAmplitude\" -> 1.0*^-6
  \"PinkNoiseLogFactor\" -> 3.0";

PlotFrequencyResponse::usage = 
  "PlotFrequencyResponse[model, {fMin, fMax}] plots the magnitude of S-parameters (Linear scale).
   Options:
     \"Measurement\" -> \"S21\" (default) | \"S11\"
     fMin, fMax in GHz.
   Requires ComputeNumericalHarmonicPerturbation.";

PlotPotentialSlices3D::noequilibria = "No equilibrium points found. Cannot create visualization.";
PlotPotentialSlices3D::dimension = "Expected 3 flux variables, got `1`. SliceContourPlot3D requires 3D potential.";


Begin["`Private`"];

Options[PlotFrequencyResponse] = {
    "Measurement" -> "S21" (* "S11" or "S21" *)
};

Options[PlotPlasmonSpectrum] = {
  NumModes -> 2,
  FluxRange -> {0., 0.5},
  FrequencyUnit -> "GHz"
};

Options[PlotPotentialSlices3D] = {
  SliceType -> "Auto",              
  ShowEquilibriumPoints -> True,
  PlotPoints -> 25,
  Contours -> 15,
  BoundaryThreshold -> 0.45,
  PlotCenter -> "Origin"     (* НОВОЕ: "GlobalMinimum" | "Origin" | {φ1, φ2, φ3} *)
};

Options[PlotRelaxationTime] = Join[
  Options[PlotPlasmonSpectrum],
  {
    "RelaxationChannel" -> "CapacitiveRelaxationRate",
    "LogTimeRange" -> {-8, 2}
  }
];

Options[PlotDephasingTime] = Join[
  Options[PlotPlasmonSpectrum],
  {
    "LogTimeRange" -> {-8, 2}
  }
];

$DebugPlotPlasmonSpectrum = False;
$DebugPlotPotentialSlices3D = False;

PlotPlasmonSpectrum[model_Association, opts:OptionsPattern[]] := 
  Module[{freqFunc, nModes, range, scale, modeFreq, 
          t1, t2, t3, dataComputeTime, plotRenderTime},
    
    (* ════════════════════════════════════════════════════════════════ *)
    (* ПРОФИЛИРОВАНИЕ: Начало общего замера                             *)
    (* ════════════════════════════════════════════════════════════════ *)
    If[$DebugPlotPlasmonSpectrum === True,
      t1 = AbsoluteTime[];
    ];
    
    (* Получить функцию PlasmonFrequenciesVsFlux напрямую *)
    freqFunc = QED`Numeric`PlasmonFrequenciesVsFlux[model];
    
    (* Обработка опций *)
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    scale = Switch[OptionValue[FrequencyUnit],
      "GHz", 2 Pi * 10^9,
      "MHz", 2 Pi * 10^6,
      _, 1.
    ];
    
    (* Определить численную функцию для каждой моды *)
    Clear[modeFreq];
    modeFreq[i_Integer][phi_?NumericQ] := Re[freqFunc[phi][[i]]] / scale;
    
    If[$DebugPlotPlasmonSpectrum === True,
      t2 = AbsoluteTime[];
      dataComputeTime = (t2 - t1) * 1000;
    ];
    
    (* ════════════════════════════════════════════════════════════════ *)
    (* ПРОФИЛИРОВАНИЕ: Рендеринг графика                                *)
    (* ════════════════════════════════════════════════════════════════ *)
    
    (* Построить график *)
    Module[{plot},
      plot = Plot[
        Evaluate @ Table[modeFreq[i][phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        PlotLegends -> Table[Subscript["\[Omega]", i], {i, nModes}],
        Frame -> True,
        FrameLabel -> {
          "\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(ext\)]\)/\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(0\)]\)",
          "Frequency (GHz)"
        },
        PlotRange -> All,
        PlotPoints -> 25,
        MaxRecursion -> 1,
        AspectRatio -> 0.6,
        ImageSize -> 600,
        TicksStyle -> Directive[FontSize -> 14, FontFamily -> "Times"],
        PlotStyle -> {
          Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.006]],  (* Синий *)
          Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.006]]    (* Оранжевый *)
        },
        Frame -> True,
        FrameStyle -> Directive[FontSize -> 14, FontFamily -> "Times", Black],
        FrameLabel -> {
          Style[Subscript["Φ", "ext"] / Subscript["Φ", "0"], 16],
          Style["Frequency (GHz)", 16]
        }
        
        (* ,opts *)
      ];

      If[$DebugPlotPlasmonSpectrum === True,
        t3 = AbsoluteTime[];
        plotRenderTime = (t3 - t2) * 1000;
      ];
      
      (* ════════════════════════════════════════════════════════════════ *)
      (* ПРОФИЛИРОВАНИЕ: Вывод результатов                                *)
      (* ════════════════════════════════════════════════════════════════ *)
      If[$DebugPlotPlasmonSpectrum === True,
        Print["[PROFILE PlotPlasmonSpectrum]"];
        Print["  Data preparation: ", Round[dataComputeTime, 0.1], " ms"];
        Print["  Plot rendering: ", Round[plotRenderTime, 0.1], " ms"];
        Print["  Total time: ", Round[(t3 - t1) * 1000, 0.1], " ms"];
        Print["  NOTE: Actual computation happens during Plot evaluation"];
      ];
      plot
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
  
  equilibria = QED`Model`GetCacheEntry[
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

PlotLabMatrixElements[model_Association] := Module[
  {data, nodes, modes, makeGrid},

  (* 1. Получаем данные *)
  data = QED`Numeric`GetLabMatrixElements[model];
  
  If[FailureQ[data], 
    Return[Style["Model not diagonalized yet. Please Run Analysis.", Red, Italic]]
  ];

  (* Извлекаем индексы *)
  nodes = Keys[data["Flux"]];
  modes = Keys[data["Flux"][First[nodes]]];

  (* 2. Вспомогательная функция для генерации таблицы *)
  makeGrid[type_, unit_] := Module[{header, rows, val},
    
    (* Определяем количество мод для формирования векторов *)
    numModes = Length[modes];

    (* Заголовок: Node \ State |1,0,0> |0,1,0> ... *)
    header = Prepend[
      Table[
        (* Создаем список из нулей и ставим 1 на место текущей моды m *)
        (* Пример: для моды 2 из 3 это будет {0, 1, 0} *)
        stateVec = ReplacePart[ConstantArray[0, numModes], m -> 1];
        
        (* Формируем строку вида "|0,1,0>" *)
        label = "|" <> StringRiffle[ToString /@ stateVec, ","] <> ">";
        
        Style[label, Bold, Darker[Blue]], 
        {m, modes}
      ],
      Style["Node \\ State", Bold, Italic]
    ];

    (* Строки данных *)
    rows = Table[
      Prepend[
        Table[
          val = data[type][n][m];
          (* Форматирование: 3 значащие цифры, научная нотация *)
          Item[
            ScientificForm[val, 3], 
            Alignment -> Center
          ],
          {m, modes}
        ],
        Style["Node " <> ToString[n], Bold]
      ],
      {n, nodes}
    ];

    (* Сборка Grid *)
    Grid[
      Prepend[rows, header], 
      Frame -> All, 
      FrameStyle -> LightGray,
      Background -> {
         {1 -> LightGray}, 
         {1 -> LightGray}, 
         {1, 1} -> White
      },
      Spacings -> {1.5, 1.2},
      ItemSize -> {Automatic, Automatic}
    ]
  ];

  (* 3. Формируем единую колонку с двумя таблицами *)
  Column[{
    (* --- FLUX SECTION --- *)
    Text[Style["Transition Matrix Elements (Flux) <0|\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(i\)]\)|1k>", Medium, Bold]], 
    Text[Style["Unit: " <> data["Units"]["Flux"] <> " (Weber)", Gray]],
    Spacer[10],
    makeGrid["Flux", "Wb"],
    
    Spacer[30], (* Отступ между таблицами *)
    
    (* --- CHARGE SECTION --- *)
    Text[Style["Transition Matrix Elements (Charge) <0|\!\(\*SubscriptBox[\(Q\), \(i\)]\)|1k>", Medium, Bold]], 
    Text[Style["Unit: " <> data["Units"]["Charge"] <> " (Coulomb)", Gray]],
    Spacer[10],
    makeGrid["Charge", "C"]
  }, Alignment -> Center]
];

PlotFermiRates[model_Association, opts:OptionsPattern[]] := Module[
  {data, modes, freqs, indGamma, capGamma, totalT1, rows, header, formatTime},
  
  data = QED`Numeric`CalculateFermiRates[model];
  
  If[FailureQ[data], Return[Style["Error calculating rates.", Red]]];

  modes = data["Modes"];
  freqs = data["Frequencies"];
  indGamma = data["InductiveRelaxationRate"];
  capGamma = data["CapacitiveRelaxationRate"];
  totalT1 = data["TotalT1"];

  formatTime[rate_] := If[rate <= 1.0*^-20, Infinity, ScientificForm[1.0 / rate * 10^6, 3]];
  formatTotalTime[t_] := If[t === Infinity, Infinity, ScientificForm[t * 10^6, 3]];

(* 2. Формируем таблицу *)
  header = {
    Style["Mode (Freq)", Bold, Darker[Blue]], (* Обновили заголовок *)
    Style["\!\(\*SubscriptBox[\(T\), \(1\)]\) Inductive\n(RL, \[Mu]s)", Bold],
    Style["\!\(\*SubscriptBox[\(T\), \(1\)]\) Capacitive\n(RC, \[Mu]s)", Bold],
    Style["Total \!\(\*SubscriptBox[\(T\), \(1\)]\)\n(\[Mu]s)", Bold]
  };

  rows = Table[
    {
      (* Теперь в первом столбце и индекс, и частота в ГГц *)
      Row[{
        Style["#" <> ToString[m], Bold], 
        " (", 
        N[freqs[[m]] / (2 Pi * 10^9), 3], 
        " GHz)"
      }],
      formatTime[indGamma[[m]]],
      formatTime[capGamma[[m]]],
      formatTotalTime[totalT1[[m]]]
    },
    {m, modes}
  ];

  Column[{
    Text[Style["Relaxation Times (Coupling to 50 \[CapitalOmega] Ports)", Large, Bold]],
    Text[Style["Inductive: M=2pH | Capacitive: Cc=1fF", Gray]],
    Spacer[10],
    Grid[
      Prepend[rows, header], 
      Frame -> All, 
      FrameStyle -> LightGray,
      Background -> {{1 -> LightGray}, {1 -> LightGray}, {1, 1} -> White},
      Spacings -> {1.5, 1.2},
      Alignment -> Center
    ]
  }, Alignment -> Center]
];

PlotDephasingRates[model_Association, opts:OptionsPattern[]] := Module[
  {
    data, nModes, freqs, d1, d2, 
    A, logFac, 
    gamma1, gamma2, t1, t2, tTot,
    rows, header, formatTime
  },
  
  (* 1. Выполняем расчет *)
  data = QED`Numeric`CalculateDephasingRates[model];
  
  If[FailureQ[data] || !AssociationQ[data], 
    Return[Style["Error: Could not calculate dephasing rates. Check model initialization.", Red]]
  ];

  (* 2. Извлекаем параметры для пересчета компонент *)
  (* Нам нужно восстановить отдельные вклады T1 и T2, так как функция вернула только Total *)
  (* Берем те же опции, что использовались по умолчанию или переданы *)
  
  (* ВАЖНО: CalculateDephasingRates возвращает dOmega/dPhi уже нормированные на квант потока, 
     а параметры шума берем из опций функции CalculateDephasingRates *)
     
  A = OptionValue[QED`Numeric`CalculateDephasingRates, {opts}, "FluxNoiseAmplitude"];
  logFac = OptionValue[QED`Numeric`CalculateDephasingRates, {opts}, "PinkNoiseLogFactor"];
  
  (* Дефолты, если не переданы *)
  If[!NumericQ[A], A = 1.0*^-6];
  If[!NumericQ[logFac], logFac = 3.0];
  
  (* 3. Данные из результата *)
  freqs = data["Frequencies"]; (* rad/s *)
  d1 = data["dOmega_dPhi"];    (* rad/s per Phi0 *)
  d2 = data["d2Omega_dPhi2"];  (* rad/s per Phi0^2 *)
  
  nModes = Length[freqs];
  
  (* 4. Расчет компонент *)
  (* Gamma1 = A * log * |d1| *)
  gamma1 = A * logFac * Abs[d1];
  
  (* Gamma2 = A^2 * log * |d2| *)
  gamma2 = (A^2) * logFac * Abs[d2];
  
  (* Времена (защита от деления на 0) *)
  formatTime[g_] := If[g < 1.0*^-20, Infinity, ScientificForm[1.0/g, 3]];
  
  (* 5. Формирование таблицы *)
  header = {
    Style["Mode (Freq)", Bold, Darker[Blue]],
    Style["\!\(\*SubscriptBox[\(T\), \(\[Phi]\)]\) (1st Order)\n(Slope, s)", Bold],
    Style["\!\(\*SubscriptBox[\(T\), \(\[Phi]\)]\) (2nd Order)\n(Curvature, s)", Bold],
    Style["Total \!\(\*SubscriptBox[\(T\), \(\[Phi]\)]\)\n(s)", Bold]
  };
  
  rows = Table[
    {
      Row[{
        Style["#" <> ToString[m], Bold], 
        " (", N[freqs[[m]] / (2 Pi * 10^9), 3], " GHz)"
      }],
      formatTime[gamma1[[m]]],
      formatTime[gamma2[[m]]],
      Style[formatTime[Sqrt[gamma1[[m]]^2 + gamma2[[m]]^2]], Bold] 
    },
    {m, nModes}
  ];
  
  Column[{
    Text[Style["Pure Dephasing Times (1/f Flux Noise)", Large, Bold]],
    Text[Style[Row[{"Noise Amplitude: ", ScientificForm[A], " \!\(\*SubscriptBox[\(\[CapitalPhi]\), \(0\)]\)"}], Gray]],
    Spacer[10],
    Grid[
      Prepend[rows, header], 
      Frame -> All, 
      FrameStyle -> LightGray,
      Background -> {{1 -> LightGray}, {1 -> LightGray}, {1, 1} -> White},
      Spacings -> {2, 1.5},
      Alignment -> Center,
      ItemSize -> {{15, 12, 12, 12}, Automatic}
    ]
  }, Alignment -> Center]
];

(* Копия PlotPlasmonSpectrum с заменой движка на GenerateFluxSweep *)
PlotGenericFluxSweep[model_Association, opts:OptionsPattern[]] := 
  Module[{freqFunc, nModes, range, scale, modeFreq, plot},
    
    (* 1. ИЗМЕНЕНИЕ: Используем GenerateFluxSweep вместо PlasmonFrequenciesVsFlux *)
    (* Анализатор: просто достаем частоты из "прогретой" модели *)
    freqFunc = QED`Numeric`GenerateFluxSweep[model, 
        Function[{m}, QED`Model`GetNumericalQuantity[m, "PlasmonFrequencies"]]
    ];

    (* Проверка на ошибки инициализации *)
    If[freqFunc === $Failed,
      Return[Graphics[{Red, Text["Error: Initialize model first!", {0,0}]}]]
    ];

    (* 2. ОПЦИИ: Берем те же, что у оригинала *)
    nModes = OptionValue[PlotPlasmonSpectrum, {opts}, NumModes];
    range = OptionValue[PlotPlasmonSpectrum, {opts}, FluxRange];
    scale = Switch[OptionValue[PlotPlasmonSpectrum, {opts}, FrequencyUnit],
      "GHz", 2 Pi * 10^9,
      "MHz", 2 Pi * 10^6,
      _, 1.
    ];

    (* 3. ОБРАБОТКА ДАННЫХ: Точно так же оборачиваем в modeFreq *)
    Clear[modeFreq];
    modeFreq[i_Integer][phi_?NumericQ] := Re[freqFunc[phi][[i]]] / scale;

    (* 4. ГРАФИК: Полная копия стилей PlotPlasmonSpectrum *)
    Plot[
        Evaluate @ Table[modeFreq[i][phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        (* Легенда чуть отличается, чтобы понимать где что *)
        PlotLegends -> Table[Row[{"Gen. Sweep ", Subscript["\[Omega]", i]}], {i, nModes}],
        
        (* Все визуальные настройки 1-в-1 как в оригинале *)
        Frame -> True,
        FrameLabel -> {
          "\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(ext\)]\)/\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(0\)]\)",
          "Frequency (GHz)"
        },
        PlotRange -> All,
        PlotPoints -> 25,
        MaxRecursion -> 1,
        AspectRatio -> 0.6,
        ImageSize -> 600,
        TicksStyle -> Directive[FontSize -> 14, FontFamily -> "Times"],
        
        (* СТИЛЬ: Те же цвета и толщина, но добавили Dashed для отличия *)
        PlotStyle -> {
          Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.006], Dashed],  (* Синий пунктир *)
          Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.006], Dashed]    (* Оранжевый пунктир *)
        },
        
        FrameStyle -> Directive[FontSize -> 14, FontFamily -> "Times", Black],
        FrameLabel -> {
          Style[Subscript["Φ", "ext"] / Subscript["Φ", "0"], 16],
          Style["Frequency (GHz)", 16]
        }
        
        (* ,opts *)
    ]
  ];

PlotRelaxationTime[model_Association, opts:OptionsPattern[]] := 
  Module[{t1DataFunc, nModes, range, channel, modeT1, labelSub, isRate},
    
    (* 1. Получаем настройки *)
    channel = OptionValue["RelaxationChannel"];
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    logRange = OptionValue["LogTimeRange"];

    (* Преобразуем степени в реальные значения для PlotRange *)
    timeRange = If[ListQ[logRange] && Length[logRange] == 2,
        {10.^logRange[[1]], 10.^logRange[[2]]},
        All (* Fallback, если формат нарушен *)
    ];

    (* Определяем, нужно ли инвертировать (Rate -> Time) *)
    isRate = StringContainsQ[channel, "Rate", IgnoreCase -> True];
    
    If[!IntegerQ[nModes], nModes = 1]; 
    If[!ListQ[range], range = {-0.5, 0.5}];

    (* 2. Подготовка функции свипа *)
    t1DataFunc = QED`Numeric`GenerateFluxSweep[model, 
        Function[{m}, 
            Module[{rates, rawData},
                (* Вызов функции расчета *)
                rates = QED`Numeric`CalculateFermiRates[m];
                
                (* Извлечение данных по ключу *)
                rawData = rates[channel];
                
                (* Если это Rate (Гц), а мы хотим T1 (с), нужно инвертировать.
                   Если это уже Time (TotalT1), оставляем как есть. *)
                If[isRate,
                    Map[If[TrueQ[# > 10^-20], 1.0/#, Infinity] &, rawData],
                    rawData
                ]
            ]
        ]
    ];

    If[t1DataFunc === $Failed,
      Return[Graphics[{Red, Text["Error: Initialize model first!", {0,0}]}]]
    ];

    (* 3. Обертка данных *)
    modeT1[i_Integer][phi_?NumericQ] := 
      Module[{val},
        val = t1DataFunc[phi][[i]];
        (* Фильтр для LogPlot: убираем бесконечности и нули *)
        If[!NumericQ[val] || val <= 0 || val === Infinity, Null, val]
      ];

    (* 4. Подготовка подписи *)
    labelSub = Switch[channel,
        "InductiveRelaxationRate", "ind",
        "CapacitiveRelaxationRate", "cap",
        "TotalT1", "tot",
        _, "x"
    ];

    (* 5. ГРАФИК *)
    Plot[
        Evaluate @ Table[modeT1[i][phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        ScalingFunctions -> "Log10",
        
        (* Диапазон под T1 (секунды) *)
        PlotRange -> {Automatic, {10^(-8), 10^(2)}}, 
        
        Axes -> True,
        Frame -> False,
        
        AxesLabel -> {
            Style[Subscript["\[CapitalPhi]", "ext"], FontFamily -> "Times New Roman", Large], 
            Style[Subscript["T", "1"], FontFamily -> "Times New Roman", Large],
            FormatType -> TraditionalForm
        }, 
        AxesStyle -> Directive[Black, FontSize -> 16, FontFamily -> "Times"],
        MeshFunctions -> Function[{x, y}, y],
        
        ImageSize -> 600, 
        
        PlotLegends -> Placed[
            Table[
                Row[{
                   Subscript["T", "1"]^labelSub,
                   " (", 
                   Subscript[Style["|1\[RightAngleBracket]", Italic], i],
                   " \[Rule] ", 
                   Style["|0\[RightAngleBracket]", Italic], 
                   ")"
                }], 
                {i, nModes}
            ],
            Right
        ],
        
        PlotStyle -> {
            Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.005]], 
            Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.005]]
        },
        
        MaxRecursion -> ControlActive[2, 6], 
        PlotPoints -> ControlActive[20, 80]

        (* ,opts *)
    ]
  ];

PlotDephasingTime[model_Association, opts:OptionsPattern[]] := 
  Module[{tPhiFunc, nModes, range, logRange, timeRange, modeTphi},
    
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    logRange = OptionValue["LogTimeRange"];
    
    timeRange = If[ListQ[logRange] && Length[logRange] == 2,
        {10.^logRange[[1]], 10.^logRange[[2]]},
        All
    ];

    If[!IntegerQ[nModes], nModes = 1]; 
    If[!ListQ[range], range = {-0.5, 0.5}];

    (* Генератор свипа *)
    tPhiFunc = QED`Numeric`GenerateFluxSweep[model, 
        Function[{m}, 
            Module[{res},
                res = QED`Numeric`CalculateDephasingRates[m, 
                    FilterRules[{opts}, Options[QED`Numeric`CalculateDephasingRates]]
                ];
                (* Возвращаем список времен (числа или Infinity) *)
                If[FailureQ[res], ConstantArray[Infinity, nModes], res["DephasingTime"]]
            ]
        ]
    ];

    If[tPhiFunc === $Failed,
      Return[Graphics[{Red, Text["Error: Initialize model first!", {0,0}]}]]
    ];

    (* Обертка для Plot *)
    modeTphi[i_Integer][phi_?NumericQ] := 
      Module[{valVec, val},
        valVec = tPhiFunc[phi];
        If[i > Length[valVec], Return[Null]];
        
        val = valVec[[i]];
        
        (* Infinity превращаем в Null (разрыв линии) *)
        If[!NumericQ[val] || val <= 0 || val === Infinity, Null, val]
      ];

    (* График *)
    Plot[
        Evaluate @ Table[modeTphi[i][phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        ScalingFunctions -> "Log10",
        PlotRange -> {Automatic, timeRange}, 
        
        Axes -> True,
        Frame -> False,
        
        AxesLabel -> {
            Style[Subscript["\[CapitalPhi]", "ext"], FontFamily -> "Times New Roman", Large], 
            Style[Subscript["T", "\[Phi]"], FontFamily -> "Times New Roman", Large],
            FormatType -> TraditionalForm
        }, 
        AxesStyle -> Directive[Black, FontSize -> 16, FontFamily -> "Times"],
        ImageSize -> 600, 
        
        PlotLegends -> Placed[
            Table[
                Row[{
                   Subscript["T", "\[Phi]"],
                   " (", 
                   Subscript[Style["|1\[RightAngleBracket]", Italic], i],
                   " \[Rule] ", 
                   Style["|0\[RightAngleBracket]", Italic], 
                   ")"
                }], 
                {i, nModes}
            ],
            Right
        ],
        
        PlotStyle -> {
            Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.005]], 
            Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.005]]
        },
        MaxRecursion -> ControlActive[2, 6], 
        PlotPoints -> ControlActive[20, 80]
    ]
];

PlotFrequencyResponse[model_Association, {fMin_, fMax_}, opts:OptionsPattern[]] := 
 Module[{cacheEntry, sNumExpr, sVar, plotFunc, measure, color, label},
  
  measure = OptionValue["Measurement"];
  
  (* 1. Достаем закэшированную функцию (Полусимвольную) *)
  cacheEntry = model["Numerical"]["Cache"]["SMatrixNumerical"];

  If[MissingQ[cacheEntry] || Lookup[cacheEntry, "State"] =!= "Ready",
     Return[Graphics[{
        Red, 
        Text[Style["S-Matrix not ready.\nRun Analysis first.", 14], {0,0}]
     }, ImageSize -> 400, Frame -> True]]
  ];

  sNumExpr = cacheEntry["Value"];         (* Матрица {{S11, S12}, {S21, S22}} *)
  sVar = cacheEntry["FrequencyVariable"]; (* Символ 's' *)

  (* 2. Формируем функцию для Plot *)
  (* S11 = [[1,1]], S21 = [[2,1]] *)
  (* Подставляем s -> I * 2Pi * f * 10^9. Plot будет вызывать это адаптивно. *)
  plotFunc = Switch[measure,
     "S11", Function[fGHz, Abs[ sNumExpr[[1, 1]] /. sVar -> (I * 2 * Pi * fGHz * 10^9) ]],
     "S21", Function[fGHz, Abs[ sNumExpr[[2, 1]] /. sVar -> (I * 2 * Pi * fGHz * 10^9) ]],
     _, Return[$Failed]
  ];

  (* 3. Настройка стилей (как в PlotPlasmonSpectrum) *)
  color = Switch[measure, 
     "S11", RGBColor[0.12, 0.47, 0.71], (* Синий *)
     _, RGBColor[1.0, 0.50, 0.05]       (* Оранжевый *)
  ];
  
  label = Switch[measure, "S11", "|S11| (Reflection)", _, "|S21| (Transmission)"];

  (* 4. Рисуем красивый Plot *)
  Plot[plotFunc[f], {f, fMin, fMax},
     
     (* Оформление 1-в-1 как у других графиков *)
     Frame -> True,
     FrameLabel -> {
         Style["Frequency (GHz)", 16], 
         Style["Magnitude |S|", 16]
     },
     FrameStyle -> Directive[FontSize -> 14, FontFamily -> "Times", Black],
     TicksStyle -> Directive[FontSize -> 14, FontFamily -> "Times"],
     
     PlotRange -> {0, 1.02}, (* Линейная шкала 0..1 с небольшим запасом *)
     PlotStyle -> Directive[color, Thickness[0.006]],
     
     GridLines -> Automatic,
     AspectRatio -> 0.6,
     ImageSize -> 600,
     PlotLabel -> Style[label, 14, FontFamily -> "Times"],
     
     (* Качество *)
     MaxRecursion -> 4,
     PlotPoints -> 500
  ]
 ];

End[];
EndPackage[];
