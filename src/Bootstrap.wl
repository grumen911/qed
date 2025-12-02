BeginPackage["QED`Bootstrap`"];

Initialize::usage = "Initialize[] sets up the package.";

Begin["`Private`"];

Initialize[] := Module[{},
  (* версионирование *)
  $QEDVersion = "0.1.0";
  
  (* инициализация глобального состояния *)
  $QEDCache = <||>;
  $QEDDebug = False;
  dir1 = ""
  dir2 =
  dir3 =
  dir4 =
  
  (* загрузка подсистем *)
	$srcDir = DirectoryName[$InputFileName];

	Get[FileNameJoin[{$srcDir, "Analytic", "Analytic.wl"}]];
	Get[FileNameJoin[{$srcDir, "Numeric", "Numeric.wl"}]];
	Get[FileNameJoin[{$srcDir, "Model.wl"}]];
	Get[FileNameJoin[{$srcDir, "GUI", "GUI.wl"}]];
	Get[FileNameJoin[{$srcDir, "PlotStyle", "PlotStyle.wl"}]];
  
  (* валидация *)
  If[! StringQ[$QEDVersion], Message[Initialize::error, "Version not set"]];
];

End[];
EndPackage[];