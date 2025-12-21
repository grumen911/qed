BeginPackage["QED`Plots`PlasmonSpectrum`"];

PlotPlasmonSpectrum::usage = 
  "PlotPlasmonSpectrum[model] строит график зависимости плазмонных частот \
от внешнего магнитного потока Φext. Использует GetNumericalQuantity для \
получения функции ω[φext].

Options:
  NumModes -> 2 (default) — количество мод для отображения
  FluxRange -> {-0.5, 0.5} — диапазон внешнего потока в единицах Φ₀
  FrequencyUnit -> \"GHz\" | \"MHz\" | \"rad/s\" — единицы частоты
  ImageSize -> 500 — размер изображения
  PlotStyle -> Automatic — стиль линий

Returns:
  Plot объект или $Failed если PlasmonFrequenciesVsFlux недоступен.

Physics:
  Отображает собственные частоты ω_i(Φext) гармонического приближения,
  где Φext — внешний магнитный поток через петлю схемы.
  
Reference: Koch et al., PRA 76, 042319 (2007), Eq. 8";

PlotPlasmonSpectrum::toomany = 
  "Requested `1` modes but model has only `2`. Plotting all available.";

Begin["`Private`"];

Options[PlotPlasmonSpectrum] = {
  NumModes -> 2,
  FluxRange -> {-0.5, 0.5},
  FrequencyUnit -> "GHz",
  ImageSize -> 500,
  PlotStyle -> Automatic,
  PlotLegends -> Automatic
};

PlotPlasmonSpectrum[model_Association, opts:OptionsPattern[]] := Module[
  {
    ωFunc, nModes, fluxRange, freqUnit, imgSize, plotStyle, 
    scaleFactor, unitLabel, frequencies, legends, 
    ωTest, nModesAvailable
  },
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ИЗВЛЕЧЕНИЕ ФУНКЦИИ ω(φext)                                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  ωFunc = QED`Model`GetNumericalQuantity[model, "PlasmonFrequenciesVsFlux"];
  If[ωFunc === $Failed, Return[$Failed]];
  
  (* Проверка числа доступных мод *)
  ωTest = ωFunc[0.0];
  If[ωTest === $Failed, Return[$Failed]];
  nModesAvailable = Length[ωTest];
  
  (* Опции *)
  nModes    = OptionValue[NumModes];
  fluxRange = OptionValue[FluxRange];
  freqUnit  = OptionValue[FrequencyUnit];
  imgSize   = OptionValue[ImageSize];
  plotStyle = OptionValue[PlotStyle];
  
  (* Ограничить запрос доступным числом мод *)
  If[nModes > nModesAvailable,
    Message[PlotPlasmonSpectrum::toomany, nModes, nModesAvailable];
    nModes = nModesAvailable
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* МАСШТАБИРОВАНИЕ ЧАСТОТЫ                                          *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  {scaleFactor, unitLabel} = Switch[freqUnit,
    "GHz",   {2 Pi * 10^9, "Frequency (GHz)"},
    "MHz",   {2 Pi * 10^6, "Frequency (MHz)"},
    "rad/s", {1., "ω (rad/s)"},
    _,       {2 Pi * 10^9, "Frequency (GHz)"}
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ПОСТРОЕНИЕ ЧАСТОТ                                                *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  frequencies = Table[
    Re[ωFunc[φ][[i]]] / scaleFactor,
    {i, nModes}
  ];
  
  (* Легенды *)
  legends = If[OptionValue[PlotLegends] === Automatic,
    Table[Subscript["ω", i], {i, nModes}],
    OptionValue[PlotLegends]
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ГРАФИК                                                           *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Plot[
    Evaluate[frequencies],
    {φ, fluxRange[[1]], fluxRange[[2]]},
    
    PlotLegends -> legends,
    PlotStyle -> plotStyle,
    PlotRange -> All,
    PlotLabel -> "Plasmon Spectrum vs External Flux",
    ImageSize -> imgSize,
    
    Frame -> True,
    FrameLabel -> {
      Row[{Subscript["Φ", "ext"], "/", Subscript["Φ", "0"]}],
      unitLabel
    },
    
    GridLines -> Automatic,
    GridLinesStyle -> Directive[Gray, Dashed, Opacity[0.3]]
  ]
];

End[];
EndPackage[];