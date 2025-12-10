BeginPackage["QED`Bootstrap`"];

Initialize::usage = "Initialize[] sets up the package.";

Begin["`Private`"];

Initialize[] := Module[{},
  (* версионирование *)
  $QEDVersion = "0.1.0";
  
  (* инициализация глобального состояния *)
  $QEDCache = <||>;
  $QEDDebug = False;
  
  (* Загрузка подсистем (порядок важен!) *)
  $srcDir = DirectoryName[$InputFileName];
  
  (* Интерактивный модуль (зависит от Model и GUI) *)
  Get[FileNameJoin[{$srcDir, "Interactive.wl"}]];
  
  (* модель (зависит от Analytic и Numeric) *)
  Get[FileNameJoin[{$srcDir, "Model.wl"}]];
  
  (* Интерактивный модуль (зависит от Model и GUI) *)
  Get[FileNameJoin[{$srcDir, "CircuitTopology.wl"}]];
  
  (* загружаем аналитические функции *)
  Get[FileNameJoin[{$srcDir, "Analytic.wl"}]];
  
  (* численные *)
  Get[FileNameJoin[{$srcDir, "Numeric.wl"}]];
  
  (* стиль *)
  Get[FileNameJoin[{$srcDir, "PlotStyle.wl"}]];
  
  (* GUI (зависит от Model) *)
  Get[FileNameJoin[{$srcDir, "GUI.wl"}]];
  

  
  (* валидация *)
  If[! StringQ[$QEDVersion], Message[Initialize::error, "Version not set"]];
];

End[];
EndPackage[];