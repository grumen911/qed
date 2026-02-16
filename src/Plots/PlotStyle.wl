BeginPackage["QED`Style`"];

QubitPlot::usage = "QubitPlot[expr, range] plots with default styling.";
DefaultPlotOptions::usage = "DefaultPlotOptions[key] returns plot options.";
ApplyExportPreset::usage = "ApplyExportPreset[graphics, presetName] applies styling rules (Screen/Publication).";

Begin["`Private`"];

DefaultPlotOptions["QubitTimeSeries"] := {
  PlotRange -> All,
  Frame -> True,
  PlotStyle -> {Thick, ColorData[97][1]},
  ImageSize -> 300
};

QubitPlot[expr_, {t_, tmin_, tmax_}, opts___] :=
  Plot[expr, {t, tmin, tmax},
    Evaluate @ DefaultPlotOptions["QubitTimeSeries"],
    opts
  ];

(* === NEW EXPORT LOGIC === *)

(* 1. Screen: Возвращаем как есть *)
ApplyExportPreset[g_, "Screen"] := g;

(* 2. Publication: Делаем "жирным" и читаемым *)
ApplyExportPreset[g_, "Publication"] := 
  Module[{styledG},
    (* Шаг А: Увеличиваем толщину линий в 2 раза *)
    styledG = g /. {
       Thickness[t_?NumericQ] :> Thickness[2.5 * t], 
       AbsoluteThickness[t_] :> AbsoluteThickness[2 * t]
    };
    
    (* Шаг Б: Форсируем стили рамки и шрифтов через Show *)
    (* Используем Prolog/Epilog для сохранения меток, если они были *)
    Show[styledG,
       BaseStyle -> {FontFamily -> "Arial", FontSize -> 14},
       FrameStyle -> Directive[Black, AbsoluteThickness[1.5], FontSize -> 14],
       AxesStyle -> Directive[Black, AbsoluteThickness[1.5], FontSize -> 14],
       (* Фиксированный размер для предсказуемости в Illustrator (напр. 4 дюйма шириной) *)
       ImageSize -> 72 * 5 
    ]
  ];

End[];
EndPackage[];