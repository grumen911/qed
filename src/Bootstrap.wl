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
  
  (* 1. Сначала загружаем аналитические функции *)
  Get[FileNameJoin[{$srcDir, "Analytic.wl"}]];
  
  (* 2. Затем численные *)
  Get[FileNameJoin[{$srcDir, "Numeric.wl"}]];
  
  (* 2.5 Интерактивный модуль (зависит от Model и GUI) *)
  Get[FileNameJoin[{$srcDir, "CircuitTopology.wl"}]];
  
  (* 3. Затем модель (зависит от Analytic и Numeric) *)
  Get[FileNameJoin[{$srcDir, "Model.wl"}]];
  
  (* 4. Затем стиль *)
  Get[FileNameJoin[{$srcDir, "PlotStyle.wl"}]];
  
  (* 5. GUI (зависит от Model) *)
  Get[FileNameJoin[{$srcDir, "GUI.wl"}]];
  
  (* 6. Интерактивный модуль (зависит от Model и GUI) *)
  Get[FileNameJoin[{$srcDir, "Interactive.wl"}]];
  
  (* валидация *)
  If[! StringQ[$QEDVersion], Message[Initialize::error, "Version not set"]];
];

End[];
EndPackage[];