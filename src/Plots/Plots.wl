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

Begin["`Private`"];

Options[PlotPlasmonSpectrum] = {
  NumModes -> 2,
  FluxRange -> {0., 0.5},
  FrequencyUnit -> "GHz"
};

$DebugPlotPlasmonSpectrum = False;

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


End[];
EndPackage[];
