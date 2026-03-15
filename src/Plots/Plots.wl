BeginPackage["QED`Plots`"];

PlotSpectroscopyScanner::usage = "PlotSpectroscopyScanner[model] displays a real-time table \
of the lowest energy levels (spectroscopy), including their frequencies and photon number assignments.";

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

PlotSParameterMap::usage = 
"PlotSParameterMap[model, {fMin, fMax}] creates a density plot (heatmap) of S-parameters magnitude.
Axes: X = External Flux (Φ/Φ₀), Y = Frequency (GHz).

Options:
  FluxRange -> {-0.5, 0.5}
  \"Measurement\" -> \"S21\" (default) | \"S11\"
  PlotPoints -> 50 
  ColorFunction -> \"TemperatureMap\"
  
Performance:
  Pre-calculates symbolic S-matrix to enable fast flux sweeping.";

PlotSParameterMapCustomMesh::usage =
"PlotSParameterMapCustomMesh[model, {fMin, fMax}] creates a density plot of S-parameters on a custom non-uniform mesh.

Options:
  FluxRange -> {-0.5, 0.5}
  \"Measurement\" -> \"S21\" (default) | \"S11\"
  PlotPoints -> {100, 400}
  ColorFunction -> \"TemperatureMap\"

Performance:
  Pre-calculates symbolic S-matrix to enable fast flux sweeping.";

PlotClassicalSParameterMap::usage = 
"PlotClassicalSParameterMap[model, sweepParam, options] creates a density plot of S-parameters \
while sweeping a classical component value (e.g., L or C) and frequency.";

PlotBICModes::usage = "PlotBICModes[model, options] plots the dispersion curves of the two polynomial BIC conditions. \
Intersections of these curves indicate the presence of a Bound State in the Continuum.";

PlotPotentialSlices3D::noequilibria = "No equilibrium points found. Cannot create visualization.";
PlotPotentialSlices3D::dimension = "Expected 3 flux variables, got `1`. SliceContourPlot3D requires 3D potential.";


Begin["`Private`"];


Options[PlotBICModes] = {
  Ports -> "{1, 2}",
  SweepRange -> {0., 0.5},
  SweepPoints -> 100
};

Options[PlotSParameterMap] = {
  "FrequencyRange" -> {4., 12.},
  "FluxRange" -> {0., 0.5},
  "Measurement" -> "S21",
  "Ports" -> "{1,2}",
  PlotPoints -> 50,
  ColorFunction -> "SunsetColors",
  FrameLabel -> {
    Style[Row[{Subscript["\[CapitalPhi]", "ext"], " (", Subscript["\[CapitalPhi]", "0"], ")"}], 16],
    Style["Frequency (GHz)", 16]
  }
};

PlotClassicalSParameterMap::nosweep = "You must specify a \"SweepParameter\" option.";

Options[PlotClassicalSParameterMap] = {
  "SweepParameter" -> Subscript[QED`$JosephsonEnergySymbol, 1],
  "FrequencyRange" -> {0.01, 20.},
  "ParameterLabel" -> Row[{Subscript["L", 1], " (nH)"}],
  "ParameterRange" -> {0.01, 8.},
  "ParameterMultiplier" -> 10^-9,
  "Measurement" -> "S21",
  "Ports" -> "{1,4}",
  "ConvertFromInductance" -> True,
  "AssumeZeroFlux" -> True,
  "IgnoreJunctionCapacitance" -> True,
  PlotPoints -> 50,
  ColorFunction -> "SunsetColors"
};

Options[PlotFrequencyResponse] = {
  "FrequencyRange" -> {0., 20.}, 
  "FluxRange" -> {0., 0.5}, 
  "Measurement" -> "S21", (* "S11" or "S21" *) 
  "Ports" -> "{1,2}",
  PlotPoints -> 50
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
    "LogTimeRange" -> {Automatic, -1}(* {-8, 2} *)
  }
];

Options[PlotDephasingTime] = Join[
  Options[PlotPlasmonSpectrum],
  {
    "LogTimeRange" -> {-6, -2}
  }
];

Options[PlotSParameterMapCustomMesh] = {
  "FrequencyRange" -> {4., 12.},
  "FluxRange" -> {0., 0.5},
  "Measurement" -> "S21",
  "Ports" -> "{1,2}",
  PlotPoints -> {100, 400}, (* Теперь по умолчанию правильное разрешение *)
  "ResonanceGuide" -> None, 
  ColorFunction -> "SunsetColors",
  FrameLabel -> {
    Style[Row[{Subscript["\[CapitalPhi]", "ext"], " (", Subscript["\[CapitalPhi]", "0"], ")"}], 16],
    Style["Frequency (GHz)", 16]
  }
};

$DebugPlotPlasmonSpectrum = False;
$DebugPlotPotentialSlices3D = False;

PlotPlasmonSpectrum[model_Association, opts:OptionsPattern[]] := 
Module[{freqFunc, nModes, range, scale, modeFreq},
    
    freqFunc = QED`Numeric`GenerateSweepPipeline[model, "PlasmonFrequencies"];
    
    (* Защита от пустой модели *)
    If[freqFunc === $Failed,
      Return[Graphics[{Red, Text["Error: Sweep generation failed. Check model initialization.", {0,0}]}]]
    ];

    (* Обработка опций *)
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    scale = Switch[OptionValue[FrequencyUnit],
      "GHz", 2 Pi * 10^9,
      "MHz", 2 Pi * 10^6,
      _, 1.
    ];

    (* Сортируем частоты по возрастанию и берем i-ю моду *)
    modeFreq[i_Integer][phi_?NumericQ] := Module[{w = Re[freqFunc[phi]]},
        Sort[w][[i]] / scale
    ];

    Plot[
        Evaluate @ Table[modeFreq[i][phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        PlotLegends -> Placed[
            LineLegend[
                Automatic,
                Table["Mode " <> ToString[i], {i, nModes}],
                LegendFunction -> (Framed[#, Background -> White, FrameMargins -> 2, FrameStyle -> GrayLevel[0.6]] &)
            ],
            {Left, Bottom} (* Размещаем внизу, так как кривые обычно идут вверх *)
        ],
        Frame -> True,
        FrameLabel -> {
          Row[{Style[Subscript["\[CapitalPhi]", "ext"], FontFamily -> "Times", Italic], Style[" (", FontFamily -> "Arial"], Style[Subscript["\[CapitalPhi]", "0"], FontFamily -> "Times", Italic], Style[")", FontFamily -> "Arial"]}],
          Style["Frequency (GHz)", FontFamily -> "Arial"]
        },
        PlotRange -> Automatic,
        PlotPoints -> 250,
        MaxRecursion -> 3,
        AspectRatio -> 0.6,
        ImageSize -> 600,
        TicksStyle -> Directive[FontSize -> 14],
        PlotStyle -> {
          Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.006]],  (* Синий *)
          Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.006], Dashed]    (* Оранжевый *)
        },
        FrameStyle -> Directive[FontSize -> 14, Black]
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
  phi0 = QED`$Phi0Value;
  
  (* Независимые потоки *)
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ 
    Cases[topology["Nodes"], Except[topology["GroundNode"]]];

  (* Проверка размерности *)
  If[Length[fluxVars] != 3,
    Message[PlotPotentialSlices3D::dimension, Length[fluxVars]];
    Return[$Failed]
  ];

  (* Достаем готовый символьный потенциал и применяем строгие правила текущей модели *)
  potential = model["Analytical"]["Potential"] /. QED`Model`GetStaticRules[model];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* 2. ПОЛУЧЕНИЕ РАВНОВЕСНЫХ ТОЧЕК  *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Module[{rawFluxes, fluxList},
    (* 1. Достаем чистые векторы из новой архитектуры *)
    rawFluxes = QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
    
    If[MissingQ[rawFluxes] || FailureQ[rawFluxes] || rawFluxes === {},
      Message[PlotPotentialSlices3D::noequilibria];
      Return[$Failed]
    ];
    
    (* 2. Если вернулся один вектор (глобальный минимум), оборачиваем его в список *)
    fluxList = If[VectorQ[rawFluxes, NumericQ], {rawFluxes}, rawFluxes];
    
    (* 3. Восстанавливаем структуру для графика, вычисляя энергию на лету *)
    equilibria = <|
      "Solutions" -> Map[
        Function[pt,
          <|
            "Fluxes" -> Thread[fluxVars -> pt], 
            "Energy" -> (potential /. Thread[fluxVars -> pt])
          |>
        ],
        fluxList
      ]
    |>;
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

  Module[{ord = Ordering[freqs]},
    rows = Table[
      Module[{k = ord[[m]]},
        {
          Row[{
            Style["#" <> ToString[m], Bold], 
            " (", N[freqs[[k]] / (2 Pi * 10^9), 3], " GHz)"
          }],
          formatTime[indGamma[[k]]],
          formatTime[capGamma[[k]]],
          formatTotalTime[totalT1[[k]]]
        }
      ],
      {m, 1, Min[Length[modes], Length[freqs]]}
    ]
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
  
  Module[{ord = Ordering[freqs]},
    rows = Table[
      Module[{k = ord[[m]]},
        {
          Row[{
            Style["#" <> ToString[m], Bold], 
            " (", N[freqs[[k]] / (2 Pi * 10^9), 3], " GHz)"
          }],
          formatTime[gamma1[[k]]],
          formatTime[gamma2[[k]]],
          Style[formatTime[Sqrt[gamma1[[k]]^2 + gamma2[[k]]^2]], Bold] 
        }
      ],
      {m, 1, Min[nModes, Length[freqs]]}
    ]
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

PlotRelaxationTime[model_Association, opts:OptionsPattern[]] := 
  Module[{
    nModes, range, logRange, timeRange, channel, labelSub, 
    nodes, numNodes, fluxVars, chargeVars, hbar,
    currentOpNum, currentGradient, voltageOpsNum, voltageOp, voltageGradient, portNode,
    mInd, rInd, cCap, rCap,
    sweepFunc, computeT1, lastPhi = "Init", lastRates = {}, getRate
  },
    
    (* 1. Опции графика *)
    channel = OptionValue["RelaxationChannel"];
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    logRange = OptionValue["LogTimeRange"];
    
    timeRange = If[ListQ[logRange] && Length[logRange] == 2,
        (* Проходимся по каждому элементу списка: если число -> 10^x, иначе -> Automatic *)
        If[NumericQ[#], 10.^#, Automatic] & /@ logRange,
        All
    ];
    
    If[!IntegerQ[nModes], nModes = 1]; 
    If[!ListQ[range], range = {-0.5, 0.5}];

    (* 2. Извлекаем статические параметры и градиенты операторов (1 раз до цикла) *)
    nodes = Cases[model["Topology"]["Nodes"], Except[model["Topology"]["GroundNode"]]];
    numNodes = Length[nodes];
    fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
    chargeVars = Subscript[QED`$ChargeSymbol, #] & /@ nodes;
    hbar = QED`$hbarValue;
    
    (* Параметры шума (берем дефолты из CalculateFermiRates) *)
    mInd = OptionValue[QED`Numeric`CalculateFermiRates, FilterRules[{opts}, Options[QED`Numeric`CalculateFermiRates]], "MutualInductance"];
    rInd = OptionValue[QED`Numeric`CalculateFermiRates, FilterRules[{opts}, Options[QED`Numeric`CalculateFermiRates]], "InductiveLineResistance"];
    cCap = OptionValue[QED`Numeric`CalculateFermiRates, FilterRules[{opts}, Options[QED`Numeric`CalculateFermiRates]], "CouplingCapacitance"];
    rCap = OptionValue[QED`Numeric`CalculateFermiRates, FilterRules[{opts}, Options[QED`Numeric`CalculateFermiRates]], "CapacitiveLineResistance"];
    portNode = OptionValue[QED`Numeric`CalculateFermiRates, FilterRules[{opts}, Options[QED`Numeric`CalculateFermiRates]], "PortNode"];

    (* Операторы *)
    currentOpNum = QED`Model`GetNumericalQuantity[model, "CurrentOperatorNumerical"];
    If[FailureQ[currentOpNum], currentOpNum = 0];
    currentGradient = D[currentOpNum, {fluxVars}]; 
    
    voltageOpsNum = QED`Model`GetNumericalQuantity[model, "VoltageOperatorsNumerical"];
    If[FailureQ[voltageOpsNum], voltageOpsNum = <||>];
    voltageOp = If[KeyExistsQ[voltageOpsNum, portNode], voltageOpsNum[portNode], 0];
    voltageGradient = D[voltageOp, {chargeVars}];

    (* 3. Создаем новый JIT-конвейер для диагонализации *)
    sweepFunc = QED`Numeric`GenerateSweepPipeline[model, "HarmonicDiagonalization"];
    If[sweepFunc === $Failed,
      Return[Graphics[{Red, Text["Error: JIT Sweep generation failed.", {0,0}]}]]
    ];

    (* 4. Внутренняя функция расчета T1 по свежим матрицам *)
    computeT1[phi_] := Module[
      {diag, freqs, caps, Nmat, Mmat, rates, w, fZPF, cZPF, iElem, vElem, gInd, gCap, gTot, targetRate, ord, k},
      
      diag = sweepFunc[phi];
      If[FailureQ[diag], Return[ConstantArray[Null, nModes]]];
      
      freqs = diag["NormalModeFrequencies"];
      caps = diag["EffectiveCapacitances"];
      Nmat = diag["FluxTransform"];
      Mmat = diag["ChargeTransform"];
      
      (* ПОЛУЧАЕМ ИНДЕКСЫ ПО ВОЗРАСТАНИЮ ЧАСТОТЫ *)
      ord = Ordering[freqs];
      
      rates = Table[
        k = ord[[idx]]; (* Берем физически правильную моду *)
        w = freqs[[k]];
        
        If[TrueQ[w == 0], 
           Null,
           fZPF = Sqrt[hbar / (2.0 * caps[[k]] * w)];
           cZPF = Sqrt[(hbar * caps[[k]] * w) / 2.0];
           
           iElem = Sum[currentGradient[[n]] * Nmat[[n, k]] * fZPF, {n, 1, numNodes}];
           vElem = Sum[voltageGradient[[n]] * (-I * Mmat[[n, k]] * cZPF), {n, 1, numNodes}];
           
           gInd = (2.0 * w * mInd^2 * Abs[iElem]^2) / (hbar * rInd);
           gCap = (2.0 * rCap * cCap^2 * w * Abs[vElem]^2) / hbar;
           gTot = gInd + gCap;
           
           targetRate = Switch[channel,
               "InductiveRelaxationRate", gInd,
               "CapacitiveRelaxationRate", gCap,
               "TotalT1", gTot,
               _, gTot
           ];
           
           If[targetRate < 1.0*^-20, Null, 1.0 / targetRate]
        ],
        {idx, 1, nModes}
      ];
      rates
    ];

    (* 5. Умный кэш для Plot (чтобы не диагонализовать 2 раза для одной точки X) *)
    getRate[i_Integer, phi_?NumericQ] := (
       If[phi =!= lastPhi,
          lastPhi = phi;
          lastRates = computeT1[phi];
       ];
       If[i <= Length[lastRates], lastRates[[i]], Null]
    );

    labelSub = Switch[channel, "InductiveRelaxationRate", "ind", "CapacitiveRelaxationRate", "cap", "TotalT1", "tot", _, "x"];
    
    (* 6. График *)
    Plot[
        Evaluate @ Table[getRate[i, phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        ScalingFunctions -> "Log10",
        PlotRange -> {Automatic, timeRange}, 
        
        (* 1. ЗАКРЫТАЯ РАМКА ВМЕСТО ОТКРЫТЫХ ОСЕЙ *)
        Axes -> False,
        Frame -> True,
        FrameLabel -> {
          Row[{Style[Subscript["\[CapitalPhi]", "ext"], FontFamily -> "Times", Italic], Style[" (", FontFamily -> "Arial"], Style[Subscript["\[CapitalPhi]", "0"], FontFamily -> "Times", Italic], Style[")", FontFamily -> "Arial"]}],
          Row[{Style[Subscript["T", "1"]^labelSub, FontFamily -> "Times", Italic], Style[" (s)", FontFamily -> "Arial"]}]
        },
        FrameStyle -> Directive[Black, FontSize -> 16],
        
        MeshFunctions -> Function[{x, y}, y],
        ImageSize -> 600, 
        
        PlotLegends -> Placed[
            LineLegend[
                Automatic,
                Table["Mode " <> ToString[i], {i, nModes}],
                (* 2. СТРОГИЙ АКАДЕМИЧНЫЙ БОКС (острые углы, тонкая серая линия) *)
                LegendFunction -> (Framed[#, Background -> White, FrameMargins -> 2, FrameStyle -> GrayLevel[0.6]] &)
            ],
            {Right, Top}
        ],
        
        PlotStyle -> {
            Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.005]], 
            Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.005], Dashed]
        },
        
        MaxRecursion -> 2, 
        PlotPoints -> 250
    ]
  ];

PlotDephasingTime[model_Association, opts:OptionsPattern[]] := 
  Module[{
    nModes, range, logRange, timeRange, modeTphi,
    A, logFac, h,
    sweepFunc, computeTPhi, lastPhi = "Init", lastTimes = {}, getTPhi
  },
    
    (* 1. Опции графика *)
    nModes = OptionValue[NumModes];
    range = OptionValue[FluxRange];
    logRange = OptionValue["LogTimeRange"];
    
    timeRange = If[ListQ[logRange] && Length[logRange] == 2,
        {10.^logRange[[1]], 10.^logRange[[2]]},
        All
    ];
    
    If[!IntegerQ[nModes], nModes = 1]; 
    If[!ListQ[range], range = {-0.5, 0.5}];

    (* 2. Извлекаем параметры шума из дефолтных опций CalculateDephasingRates *)
    A = OptionValue[QED`Numeric`CalculateDephasingRates, FilterRules[{opts}, Options[QED`Numeric`CalculateDephasingRates]], "FluxNoiseAmplitude"];
    logFac = OptionValue[QED`Numeric`CalculateDephasingRates, FilterRules[{opts}, Options[QED`Numeric`CalculateDephasingRates]], "PinkNoiseLogFactor"];
    h = OptionValue[QED`Numeric`CalculateDephasingRates, FilterRules[{opts}, Options[QED`Numeric`CalculateDephasingRates]], "FluxStep"];

    (* 3. Создаем ОДИН JIT-конвейер для частот *)
    sweepFunc = QED`Numeric`GenerateSweepPipeline[model, "PlasmonFrequencies"];

    If[sweepFunc === $Failed,
      Return[Graphics[{Red, Text["Error: JIT Sweep generation failed.", {0,0}]}]]
    ];

    (* 4. Внутренняя функция: берет частоты из JIT и сама считает производные *)
    computeTPhi[phi_] := Module[
      {w0, wPlus, wMinus, d1, d2, gamma1, gamma2, gTot, ord, k},
      
      w0 = sweepFunc[phi];
      wPlus = sweepFunc[phi + h];
      wMinus = sweepFunc[phi - h];
      
      If[AnyTrue[{w0, wPlus, wMinus}, FailureQ], Return[ConstantArray[Null, nModes]]];
      
      ord = Ordering[w0];
      
      Table[
         k = ord[[idx]]; (* Берем правильный индекс *)
         If[TrueQ[w0[[k]] == 0], 
            Null,
            d1 = (wPlus[[k]] - wMinus[[k]]) / (2 * h);
            d2 = (wPlus[[k]] - 2*w0[[k]] + wMinus[[k]]) / (h^2);
            
            gamma1 = A * logFac * Abs[d1];
            gamma2 = (A^2) * logFac * Abs[d2];
            gTot = Sqrt[gamma1^2 + gamma2^2];
            
            If[gTot < 1.0*^-20 || !NumericQ[gTot], Null, 1.0 / gTot]
         ],
         {idx, 1, nModes}
      ]
    ];

    (* 5. Умный локальный кэш, чтобы не считать точки дважды для многомодовых графиков *)
    getTPhi[i_Integer, phi_?NumericQ] := (
       If[phi =!= lastPhi,
          lastPhi = phi;
          lastTimes = computeTPhi[phi];
       ];
       If[i <= Length[lastTimes], lastTimes[[i]], Null]
    );

    (* 6. График *)
    Plot[
        Evaluate @ Table[getTPhi[i, phi], {i, nModes}],
        {phi, range[[1]], range[[2]]},
        
        ScalingFunctions -> "Log10",
        PlotRange -> {Automatic, timeRange}, 
        
        Axes -> False,
        Frame -> True,
        FrameLabel -> {
          Row[{Style[Subscript["\[CapitalPhi]", "ext"], FontFamily -> "Times", Italic], Style[" (", FontFamily -> "Arial"], Style[Subscript["\[CapitalPhi]", "0"], FontFamily -> "Times", Italic], Style[")", FontFamily -> "Arial"]}],
          Row[{Style[Subscript["T", "\[Phi]"], FontFamily -> "Times", Italic], Style[" (s)", FontFamily -> "Arial"]}]
        },
        AxesStyle -> Directive[Black, FontSize -> 16],
        MeshFunctions -> Function[{x, y}, y],
        ImageSize -> 600, 
        
        PlotLegends -> Placed[
            LineLegend[
                Automatic, 
                (* Лаконичная легенда *)
                Table["Mode " <> ToString[i], {i, nModes}],
                LegendFunction -> (Framed[#, Background -> White, FrameMargins -> 2, FrameStyle -> GrayLevel[0.6]] &)
            ],
            {Right, Top}
        ],
        
        PlotStyle -> {
            Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.005]], 
            Directive[RGBColor[1.0, 0.50, 0.05], Thickness[0.005], Dashed]
        },
        
        MaxRecursion -> 2, 
        PlotPoints -> 250
    ]
];

PlotFrequencyResponse[model_Association, opts:OptionsPattern[]] := 
 Module[{measure, sIndex, color, label, phiExt, sMatrixAtPhi, plotFunc, plotPoints, fMin, fMax, portsOpt, depKey},
  
  {fMin, fMax} = OptionValue["FrequencyRange"];
  measure = OptionValue["Measurement"];
  portsOpt = OptionValue["Ports"];
  plotPoints = OptionValue[PlotPoints];
  sIndex = If[measure === "S11", {1, 1}, {2, 1}];

  (* 1. Получаем текущий внешний поток *)
  phiExt = (QED`$PhiExt /. QED`Model`GetStaticRules[model]) / QED`$Phi0Value;
  
  (* 2. Формируем ключ JIT-конвейера и безопасно вызываем его *)
  depKey = If[portsOpt === "{1,4}", "SMatrix_1_4", "SMatrix_1_2"];
  Module[{sweepFunc = QED`Numeric`GenerateSweepPipeline[model, depKey]},
    If[sweepFunc === $Failed || Head[sweepFunc] === $Failed,
       Return[Graphics[{Red, Text[Style[depKey <> " not available\n(missing nodes?)", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
    ];
    sMatrixAtPhi = sweepFunc[phiExt];
  ];
  
  If[sMatrixAtPhi === $Failed || Head[sMatrixAtPhi] === $Failed,
     Return[Graphics[{Red, Text[Style["S-Matrix evaluation failed.", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];

  (* 3. Защищенная функция (принимает только числа) *)
  plotFunc[fGHz_?NumericQ] := Abs[ sMatrixAtPhi[fGHz * 2 * Pi * 10^9][[ Sequence @@ sIndex ]] ];

  color = Switch[measure, "S11", RGBColor[0.12, 0.47, 0.71], _, RGBColor[1.0, 0.50, 0.05]];
  label = Switch[measure,
            "S11", Row[{"|", Subscript["S", "11"], "| (Reflection)"}],
            _, Row[{"|", Subscript["S", "21"], "| (Transmission)"}]
          ];

  (* 4. Отрисовка *)
  Plot[plotFunc[f], {f, fMin, fMax},
     Frame -> True,
     FrameLabel -> {Style["Frequency (GHz)", 16], Style["Magnitude |S|", 16]},
     FrameStyle -> Directive[FontSize -> 14, Black],
     TicksStyle -> Directive[FontSize -> 14],
     PlotRange -> {0, 1.02}, 
     PlotStyle -> Directive[color, Thickness[0.006]],
     GridLines -> Automatic, AspectRatio -> 0.6, ImageSize -> 600,
     PlotLabel -> Style[label, 14],
     MaxRecursion -> 10, PlotPoints -> plotPoints
  ]
 ];

PlotSParameterMap[model_Association, opts:OptionsPattern[]] :=
  Module[{
    fMin, fMax, fluxRange, measure, plotPoints, colFunc,
    sIndex, label, legendLabel, sweepFunc, plotFunc, plot, legend,
    portsOpt, depKey
  },
  
  {fMin, fMax} = OptionValue["FrequencyRange"];
  fluxRange = OptionValue["FluxRange"];
  measure = OptionValue["Measurement"];
  portsOpt = OptionValue["Ports"];

  plotPoints = OptionValue[PlotPoints];
  colFunc = OptionValue[ColorFunction];
  
  sIndex = If[measure === "S11", {1, 1}, {2, 1}];
  label = If[measure === "S11", "Reflection", "Transmission"];
  legendLabel = If[measure === "S11", Row[{"|", Subscript["S", "11"], "|"}], Row[{"|", Subscript["S", "21"], "|"}]];

  (* 1. Формируем ключ JIT-конвейера и вызываем его *)
  depKey = If[portsOpt === "{1,4}", "SMatrix_1_4", "SMatrix_1_2"];
  sweepFunc = QED`Numeric`GenerateSweepPipeline[model, depKey];
  
  If[sweepFunc === $Failed || Head[sweepFunc] === $Failed,
      Return[Graphics[{Red, Text[Style["Error: " <> depKey <> " not available\n(missing nodes?)", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];

  (* 2. Защищенная функция (принимает только числа) *)
  plotFunc[phiVal_?NumericQ, fGHz_?NumericQ] := 
    Abs[ sweepFunc[phiVal][fGHz * 2 * Pi * 10^9][[ Sequence @@ sIndex ]] ];

  (* 3. Чистый вызов DensityPlot без хэш-оберток *)
  plot = DensityPlot[
      plotFunc[phi, f], 
      {phi, fluxRange[[1]], fluxRange[[2]]}, 
      {f, fMin, fMax},
      
      PlotPoints -> {10, 40}, (* {400, 1500} *)
      Exclusions -> None,
      PerformanceGoal -> "Quality",

      PlotRange -> {0, 1.05}, 
      ColorFunction -> colFunc,
      Frame -> True,
      FrameLabel -> OptionValue[FrameLabel],
      FrameStyle -> Directive[FontSize -> 14, Black],
      PlotLabel -> Style[label, 16],
      PlotLegends -> None, 
      ImageSize -> 600,
      MaxRecursion -> 8
  ];

  (* 4. Исправленная легенда без несуществующих опций *)
  legend = BarLegend[
      {colFunc, {0, 1.05}},
      LegendLabel -> Style[legendLabel, FontSize -> 16],
      LabelStyle -> Directive[Black, 14],
      LegendMarkerSize -> {20, 300}
  ];

  (* Сборка графика и легенды *)
  Legended[plot, Placed[legend, Right]]
];

PlotSParameterMapCustomMesh[model_Association, opts:OptionsPattern[]] := 
  Module[{
    fMin, fMax, fluxRange, measure, plotPoints, colFunc,
    sIndex, label, legendLabel, sweepFunc, plot, legend,
    portsOpt, depKey,
    phiGrid, fGrid, fullDataMesh, guideFunc
  },
  
  {fMin, fMax} = OptionValue["FrequencyRange"];
  fluxRange = OptionValue["FluxRange"];
  measure = OptionValue["Measurement"];
  portsOpt = OptionValue["Ports"];
  
  plotPoints = OptionValue[PlotPoints];
  If[NumberQ[plotPoints], plotPoints = {plotPoints, plotPoints}];
  
  colFunc = OptionValue[ColorFunction];
  guideFunc = OptionValue["ResonanceGuide"];
  
  sIndex = If[measure === "S11", {1, 1}, {2, 1}];
  label = If[measure === "S11", "Reflection", "Transmission"];
  legendLabel = If[measure === "S11", Row[{"|", Subscript["S", "11"], "|"}], Row[{"|", Subscript["S", "21"], "|"}]];

  (* 1. Формируем ключ JIT-конвейера *)
  depKey = If[portsOpt === "{1,4}", "SMatrix_1_4", "SMatrix_1_2"];
  sweepFunc = QED`Numeric`GenerateSweepPipeline[model, depKey];
  
  If[sweepFunc === $Failed || Head[sweepFunc] === $Failed,
      Return[Graphics[{Red, Text[Style["Error: " <> depKey <> " not available", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];

  (* 2. Сетки *)
  phiGrid = Subdivide[fluxRange[[1]], fluxRange[[2]], plotPoints[[1]]];
  fGrid = Subdivide[fMin, fMax, plotPoints[[2]]];

  (* 3. Вычисления с защитой (как в оригинальном _?NumericQ) *)
  fullDataMesh = Flatten[
    Table[
      With[{sMatFunc = sweepFunc[phiVal]}, 
        Table[
          Module[{rawResult, zVal},
            (* Глушим варнинги деления на ноль, как это делает DensityPlot *)
            rawResult = Quiet[ sMatFunc[fVal * 2 * Pi * 10^9] ];
            
            (* Строгая проверка: убеждаемся, что вернулась именно матрица, а не мусор *)
            If[ListQ[rawResult] && Length[Dimensions[rawResult]] == 2,
              zVal = Abs[ rawResult[[Sequence @@ sIndex]] ];
              If[NumericQ[zVal],
                {phiVal, fVal, zVal},
                Nothing (* Удаляем нечисловые точки *)
              ],
              Nothing (* Если JIT движок не смог посчитать - пропускаем точку *)
            ]
          ],
          {fVal, fGrid}
        ]
      ],
      {phiVal, phiGrid} 
    ],
    1
  ];

  (* 4. Отрисовка *)
  plot = ListDensityPlot[
      fullDataMesh, 
      PlotRange -> {0, 1.05}, 
      ColorFunction -> colFunc,
      Frame -> True,
      FrameLabel -> OptionValue[FrameLabel],
      FrameStyle -> Directive[FontSize -> 14, Black],
      PlotLabel -> Style[label, 16],
      PlotLegends -> None, 
      ImageSize -> 600
  ];

  legend = BarLegend[
      {colFunc, {0, 1.05}},
      LegendLabel -> Style[legendLabel, FontSize -> 16],
      LabelStyle -> Directive[Black, 14],
      LegendMarkerSize -> {20, 300}
  ];

  Legended[plot, Placed[legend, Right]]
];

PlotClassicalSParameterMap[model_Association, opts:OptionsPattern[]] :=
  Module[{
    fMin, fMax, paramRange, measure, plotPoints, colFunc,
    sIndex, label, legendLabel, sweepFunc, plotFunc, plot, legend,
    portsOpt, depKey, paramLabel, sweepParam, paramMultiplier
  },
  
  (* Извлекаем сканируемый параметр из опций *)
  sweepParam = OptionValue["SweepParameter"];
  
  (* Защита от пустого параметра *)
  If[sweepParam === None,
      Message[PlotClassicalSParameterMap::nosweep];
      Return[Graphics[{Red, Text[Style["Error: \"SweepParameter\" is not specified.", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];

  {fMin, fMax} = OptionValue["FrequencyRange"];
  paramRange = OptionValue["ParameterRange"];
  paramMultiplier = OptionValue["ParameterMultiplier"];
  measure = OptionValue["Measurement"];
  portsOpt = OptionValue["Ports"];
  paramLabel = OptionValue["ParameterLabel"];

  plotPoints = OptionValue[PlotPoints];
  colFunc = OptionValue[ColorFunction];

  sIndex = If[measure === "S11", {1, 1}, {2, 1}];
  label = label = If[measure === "S11", "Reflection", "Transmission"];
  legendLabel = If[measure === "S11", Row[{"|", Subscript["S", "11"], "|"}], Row[{"|", Subscript["S", "21"], "|"}]];

  (* 1. Формируем ключ JIT-конвейера и вызываем НАШ КЛАССИЧЕСКИЙ генератор *)
  depKey = If[portsOpt === "{1,4}", "SMatrix_1_4", "SMatrix_1_2"];
  sweepFunc = QED`Numeric`GenerateParameterSweep[
      model, 
      depKey, 
      sweepParam, 
      "ConvertFromInductance" -> OptionValue["ConvertFromInductance"],
      "AssumeZeroFlux" -> OptionValue["AssumeZeroFlux"],
      "IgnoreJunctionCapacitance" -> OptionValue["IgnoreJunctionCapacitance"]
  ];
  
  If[sweepFunc === $Failed || Head[sweepFunc] === $Failed,
      Return[Graphics[{Red, Text[Style["Error: Sweep generation failed for " <> ToString[sweepParam], 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];

  (* 2. Защищенная функция: paramVal (ось X) и fGHz (ось Y) *)
  plotFunc[paramVal_?NumericQ, fGHz_?NumericQ] := 
    Abs[ sweepFunc[paramVal * paramMultiplier][fGHz * 2 * Pi * 10^9][[ Sequence @@ sIndex ]] ];

  (* 3. Вызов DensityPlot *)
  plot = DensityPlot[
      plotFunc[p, f], 
      {p, paramRange[[1]], paramRange[[2]]}, 
      {f, fMin, fMax},
      
      PlotPoints -> {100, 100}, 
      Exclusions -> None,
      PerformanceGoal -> "Quality",

      PlotRange -> {0, 1.05}, 
      ColorFunction -> colFunc,
      Frame -> True,
      FrameLabel -> {Style[paramLabel, 16], Style["Frequency (GHz)", 16]},
      FrameStyle -> Directive[FontSize -> 14, Black],
      PlotLabel -> Style[label, 16],
      PlotLegends -> None, 
      ImageSize -> 600,
      MaxRecursion -> 4
  ];

  (* 4. Легенда *)
  legend = BarLegend[
      {colFunc, {0, 1.05}},
      LegendLabel -> Style[legendLabel, FontSize -> 16],
      LabelStyle -> Directive[Black, 14],
      LegendMarkerSize -> {20, 300}
  ];

  Legended[plot, Placed[legend, Right]]
];

PlotSpectroscopyScanner[model_Association, opts:OptionsPattern[]] := 
  Module[{
      truncationDim = 5, (* Оптимизация для UI: 5 уровней на моду *)
      numLevels = 8,     (* Показываем первые 8 собственных чисел *)
      basis, fluxOps, hTotal, 
      evals, evecs, energies, states,
      numModes, nOps, groundEnergy, hbar,
      rows, freqStr, assignStr, nVals, rowStyle,
      diagData
  },
    
    (* 1. ПОЛУЧЕНИЕ ДАННЫХ МОДЕЛИ *)
    diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
    If[MissingQ[diagData] || FailureQ[diagData], 
       Return[Panel[Style["Model analysis failed. Check parameters.", Red], ImageSize -> {300, 50}]]
    ];
    
    numModes = Length[diagData["NormalModeFrequencies"]];
    If[numModes == 0, Return[Panel["No modes found."]]];

    (* 2. ВЫЧИСЛЕНИЕ ГАМИЛЬТОНИАНА *)
    Quiet[
        basis = QED`Numeric`GetBasisOperators[ConstantArray[truncationDim, numModes]];
        fluxOps = QED`Numeric`ConstructFluxOperators[model, basis];
        hTotal = QED`Numeric`BuildNumericalHamiltonian[model, basis, fluxOps];
    ];

    (* Защита от комплексных значений *)
    If[!FreeQ[hTotal, Complex], 
       Return[Panel[Style["Error: Hamiltonian is Complex!", Red, Bold]]]
    ];

    (* 3. ДИАГОНАЛИЗАЦИЯ *)
    {evals, evecs} = Eigensystem[hTotal, -numLevels];
    
    (* Сортировка по возрастанию энергии *)
    With[{ord = Ordering[evals]},
        energies = evals[[ord]];
        states = evecs[[ord]];
    ];

    groundEnergy = energies[[1]];
    
    (* Операторы числа фотонов для анализа состава состояний *)
    nOps = Table[basis["ad"][[k]] . basis["a"][[k]], {k, numModes}];
    hbar = QED`$hbarValue;

    (* 4. ФОРМАТИРОВАНИЕ ТАБЛИЦЫ *)
    rows = {{
        Style["Idx", Bold], 
        Style["Freq (GHz)", Bold], 
        Sequence @@ Table[Style["<n" <> ToString[k] <> ">", Bold], {k, numModes}],
        Style["State", Bold]
    }};
    
    Do[
        (* Вычисляем средние числа заполнения <n> для каждой моды *)
        nVals = Table[Re[states[[i]] . nOps[[k]] . states[[i]]], {k, numModes}];
        
        (* Частота перехода 0 -> i в ГГц *)
        freqStr = NumberForm[(energies[[i]] - groundEnergy) / hbar / 2. / Pi / 10^9, {5, 3}];
        
        (* Строковое представление состояния, например |0,1,0> *)
        assignStr = "|" <> StringRiffle[Round[nVals], ","] <> ">";
        
        (* Логика валидации: Если основное состояние (Idx=1) содержит фотоны -> ОШИБКА *)
        rowStyle = If[i == 1 && Total[nVals] > 0.15, Red, Black];
        
        AppendTo[rows, {
            Style[i, rowStyle],
            Style[freqStr, rowStyle],
            Sequence @@ (Style[NumberForm[#, {3, 2}], rowStyle] & /@ nVals),
            Style[assignStr, rowStyle]
        }];
    , {i, Length[energies]}];

    (* Возврат Grid *)
    Column[{
       Text[Style["Spectroscopy Scanner", 16, FontFamily -> "Helvetica"]],
       Text[Style["(Real-time update)", Gray, 10]],
       Spacer[5],
       Grid[rows, 
            Frame -> All, 
            Background -> {None, {1 -> LightGray}}, 
            ItemStyle -> {Automatic, Automatic},
            Alignment -> {Center, Center},
            Spacings -> {1.2, 0.8}
       ]
    }, Alignment -> Center]
  ];

PlotBICModes[model_Association, OptionsPattern[]] := Module[
  {portsOpt, phiMin, phiMax, plotPts, depKey, sweepFunc, 
   computeRoots, lastPhi = "Init", lastRoots = {{}, {}}, getEqRoot,
   maxRoots = 2, funcsToPlot, plotStyles},
  
  portsOpt = OptionValue[Ports];
  {phiMin, phiMax} = OptionValue[SweepRange];
  plotPts = OptionValue[SweepPoints];
  
  (* 1. Получаем замыкание из JIT-движка *)
  depKey = If[portsOpt === "{1,4}", "BICRoots_1_4", "BICRoots_1_2"];
  sweepFunc = QED`Numeric`GenerateSweepPipeline[model, depKey];
  
  If[sweepFunc === $Failed || Head[sweepFunc] === $Failed,
     Return[Graphics[{Red, Text[Style["BIC Analytical Roots not available\n(" <> depKey <> ")", 14], {0,0}]}, ImageSize -> 400, Frame -> True]]
  ];
  
  (* 2. Внутренняя функция расчета с умным кэшем *)
  computeRoots[phi_] := Module[{res},
    QED`Numeric`GenerateSweepPipeline[model, "SweepInit"][phi];
    res = sweepFunc[phi][0.0];
    If[ListQ[res], res / (2 * Pi * 10^9), {{}, {}}]
  ];
  
  getEqRoot[eqIdx_Integer, rootIdx_Integer, phi_?NumericQ] := (
     If[phi =!= lastPhi,
        lastPhi = phi;
        lastRoots = computeRoots[phi];
     ];
     If[eqIdx <= Length[lastRoots] && rootIdx <= Length[lastRoots[[eqIdx]]],
        lastRoots[[eqIdx, rootIdx]],
        Indeterminate
     ]
  );
  
  (* 3. Подготовка плоских структур для функции Plot *)
  (* ВАЖНО: Рисуем сначала полюса (eq=2), затем нули (eq=1), чтобы нули оказались на переднем плане *)
  funcsToPlot = Flatten[Table[getEqRoot[eq, r, phi], {eq, {2, 1}}, {r, 1, maxRoots}]];
  
  plotStyles = Flatten[Table[
    If[eq == 1, 
        Directive[RGBColor[0.12, 0.47, 0.71], Thickness[0.005]],         (* Синий сплошной для Нулей *)
        Directive[RGBColor[1.0, 0.50, 0.05], Dashed, Thickness[0.005]]   (* Оранжевый пунктир для Полюсов *)
    ],
    {eq, {2, 1}}, {r, 1, maxRoots}
  ]];
  
  (* 4. Отрисовка адаптивного графика *)
  Plot[
    Evaluate[funcsToPlot],
    {phi, phiMin, phiMax},
    
    PlotStyle -> plotStyles,
    PlotRange -> Automatic, 
    PlotLegends -> Placed[
      LineLegend[
        Automatic, 
        {"Zeros", "Poles"},
        (* Добавляем белый фон и аккуратную скругленную рамку *)
        LegendFunction -> (Framed[#, Background -> White, FrameMargins -> 2, FrameStyle -> GrayLevel[0.6]] &)
      ], 
      {Left, Bottom}
    ],
    Frame -> True,
    FrameLabel -> {
      Style[Row[{Subscript["\[CapitalPhi]", "ext"], " (", Subscript["\[CapitalPhi]", "0"], ")"}], 14], 
      Style["Frequency (GHz)", 14]
    },
    GridLines -> None,
    ImageSize -> 600,
    
    MaxRecursion -> 2,
    PlotPoints -> plotPts
  ]
];

End[];
EndPackage[];
