BeginPackage["QED`Bootstrap`"];

InitQED::usage = "Initialize[] sets up the package.";

Begin["`Private`"];

InitQED[] := Module[{srcDir},
    srcDir = DirectoryName[$InputFileName];
    If[!MemberQ[$Path, srcDir], PrependTo[$Path, srcDir]];

    (* 1. Сначала загружаем базу: стили, аналитику и МОДЕЛЬ *)
    Get[FileNameJoin[{srcDir, "PlotStyle", "PlotStyle.wl"}]];
    Get[FileNameJoin[{srcDir, "Analytic", "Analytic.wl"}]];
    Get[FileNameJoin[{srcDir, "Numeric", "Numeric.wl"}]]; 

    Get[FileNameJoin[{srcDir, "CircuitTopology.wl"}]];
    Get[FileNameJoin[{srcDir, "Model.wl"}]];

	Get[FileNameJoin[{srcDir, "GUI", "Interactive.wl"}]];

];
End[];
EndPackage[];