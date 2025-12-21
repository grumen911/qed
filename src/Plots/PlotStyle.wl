BeginPackage["QED`Style`"];

QubitPlot::usage = "QubitPlot[expr, range] plots with default styling.";
DefaultPlotOptions::usage = "DefaultPlotOptions[key] returns plot options.";

TestPlot[expr_] :=
  Plot[Sin[expr x],{x,0,1}]



Begin["`Private`"];



DefaultPlotOptions["QubitTimeSeries"] := {
  PlotRange -> All ,
  Frame -> True,
  PlotStyle -> {Thick, ColorData[97][1]},
  ImageSize -> 300
};

QubitPlot[expr_, {t_, tmin_, tmax_}, opts___] :=
  Plot[expr, {t, tmin, tmax},
    Evaluate @ DefaultPlotOptions["QubitTimeSeries"],
    opts
  ];


End[];
EndPackage[];