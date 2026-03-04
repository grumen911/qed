BeginPackage["QED`Style`"];

QubitPlot::usage = "QubitPlot[expr, range] plots with default styling.";
DefaultPlotOptions::usage = "DefaultPlotOptions[key] returns plot options.";
ApplyExportPreset::usage = "ApplyExportPreset[graphics, presetName] applies styling rules.";

Begin["`Private`"];

DefaultPlotOptions["QubitTimeSeries"] := {
  PlotRange -> All, Frame -> True, PlotStyle -> {Thick, ColorData[97][1]}, ImageSize -> 300
};

QubitPlot[expr_, {t_, tmin_, tmax_}, opts___] :=
  Plot[expr, {t, tmin, tmax}, Evaluate @ DefaultPlotOptions["QubitTimeSeries"], opts];

(* === ЛОГИКА ЭКСПОРТА === *)

ApplyExportPreset[g_, "Screen"] := g;

ApplyExportPreset[Legended[plot_, legend_], "Publication"] := 
  Module[{baseFontSize = 14, fontName = "Arial", styledPlot, styledLegend, plotSize = 72 * 5},
    
    (* 1. СТИЛИЗАЦИЯ ЛЕГЕНДЫ ПЕРЕД РАСТЕРИЗАЦИЕЙ (Для тепловых карт) *)
    styledLegend = legend /. BarLegend[arg_, opts___] :> 
       BarLegend[arg, 
          LabelStyle -> Directive[Black, baseFontSize, FontFamily -> fontName],
          LegendMarkerSize -> {20, plotSize * 0.8},
          Sequence @@ FilterRules[{opts}, Except[LabelStyle | LegendMarkerSize]]
       ];

    (* 2. РАСТЕРИЗАЦИЯ УЖЕ СТИЛИЗОВАННОЙ BARLEGEND *)
    styledLegend = styledLegend /. l_BarLegend :> 
       Image[
          Rasterize[l, "Image", ImageResolution -> 600, Background -> None], 
          ImageSize -> {Automatic, plotSize * 0.8}
       ];

    (* --- НОВЫЙ БЛОК --- *)
    (* 2.5 СТИЛИЗАЦИЯ LINELEGEND (Без растеризации, оставляем в векторе) *)
    styledLegend = styledLegend /. LineLegend[styles_, labels_, opts___] :> 
       LineLegend[styles, labels, 
          (* Шрифт чуть меньше, чем у осей (12pt вместо 14pt) *)
          LabelStyle -> Directive[Black, baseFontSize - 2, FontFamily -> fontName],
          (* Делаем цветные черточки короткими и аккуратными *)
          LegendMarkerSize -> 15,
          (* Уменьшаем вертикальный интервал между строками в легенде *)
          Spacings -> {0.5, 0.2}, 
          Sequence @@ FilterRules[{opts}, Except[LabelStyle | LegendMarkerSize | Spacings]]
       ];
    (* ------------------ *)

    (* 3. СТИЛИЗАЦИЯ ГРАФИКА *)
    styledPlot = ApplyExportPreset[plot, "Publication"];
    
    (* Собираем обратно *)
    Legended[styledPlot, styledLegend]
  ];

(* Основная функция для графиков без Legended или для внутренней части Legended *)
ApplyExportPreset[g_, "Publication"] := 
  Module[{styledG, baseFontSize = 14, fontName = "Arial", plotSize = 72 * 5},
    
    (* Масштабируем толщины линий *)
    styledG = g /. {
       Thickness[t_?NumericQ] :> Thickness[2.5 * t], 
       AbsoluteThickness[t_] :> AbsoluteThickness[2 * t]
    };

    (* Применяем общий стиль оформления *)
    Show[styledG,
       BaseStyle -> {FontFamily -> fontName, FontSize -> baseFontSize},
       FrameStyle -> Directive[Black, AbsoluteThickness[1.5], FontSize -> baseFontSize],
       AxesStyle -> Directive[Black, AbsoluteThickness[1.5], FontSize -> baseFontSize],
       (* Задаем размер внутреннего графика *)
       ImageSize -> plotSize
    ]
  ];

End[];
EndPackage[];