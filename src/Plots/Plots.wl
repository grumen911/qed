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

PlotPotentialSlices3D::noequilibria = "No equilibrium points found. Cannot create visualization.";
PlotPotentialSlices3D::dimension = "Expected 3 flux variables, got `1`. SliceContourPlot3D requires 3D potential.";


Begin["`Private`"];

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
        opts
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


End[];
EndPackage[];
