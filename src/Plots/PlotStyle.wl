(* src/Plots/PlotStyle.wl *)

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
    
    (* 1. СТИЛИЗАЦИЯ ЛЕГЕНДЫ ПЕРЕД РАСТЕРИЗАЦИЕЙ *)
    (* Мы находим BarLegend и ПРИНУДИТЕЛЬНО вшиваем в него шрифты публикации *)
    styledLegend = legend /. BarLegend[arg_, opts___] :> 
       BarLegend[arg, 
          (* Устанавливаем шрифт как у осей графика *)
          LabelStyle -> Directive[Black, baseFontSize, FontFamily -> fontName],
          (* Высота легенды должна быть чуть меньше высоты графика (0.8 от ImageSize) *)
          LegendMarkerSize -> {20, plotSize * 0.8},
          (* Пробрасываем остальные опции, исключая конфликтующие *)
          Sequence @@ FilterRules[{opts}, Except[LabelStyle | LegendMarkerSize]]
       ];

    (* 2. РАСТЕРИЗАЦИЯ УЖЕ СТИЛИЗОВАННОЙ ЛЕГЕНДЫ *)
    (* Теперь цифры в легенде будут изначально крупными *)
    styledLegend = styledLegend /. l_BarLegend :> 
       Image[
          Rasterize[l, "Image", ImageResolution -> 600, Background -> None], 
          (* ImageSize здесь отвечает за финальный размер на холсте *)
          ImageSize -> {Automatic, plotSize * 0.8}
       ];

    (* 3. СТИЛИЗАЦИЯ ГРАФИКА *)
    (* Вызываем основную функцию для отрисовки осей и линий *)
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