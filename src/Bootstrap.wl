BeginPackage["QED`Bootstrap`"];

InitQED::usage = "InitQED[] sets up the package.";
$QEDDebug::usage = "$QEDDebug - if True, enables verbose loading messages.";

Begin["`Private`"];

(* Глобальный флаг дебага *)
$QEDDebug = False;

(* Утилита для условного вывода *)
debugPrint[msg_String] := If[TrueQ[$QEDDebug], Print["[QED Debug] ", msg]];

InitQED[] := Module[{srcDir, loadTime},
    debugPrint["Starting QED initialization..."];
    
    srcDir = DirectoryName[$InputFileName];
    debugPrint["Source directory: " <> srcDir];
    
    If[!MemberQ[$Path, srcDir], 
        PrependTo[$Path, srcDir];
        debugPrint["Added to $Path: " <> srcDir];
    ];

    (* Макрос для загрузки с таймингом *)
    loadPackage[relPath_String] := Module[{file, t},
        file = FileNameJoin[{srcDir, relPath}];
        debugPrint["Loading: " <> relPath <> "..."];
        t = AbsoluteTiming[Get[file]][[1]];
        debugPrint[" [OK] Successful Loaded " <> relPath <> " in " <> ToString[NumberForm[t, {4, 3}]] <> " seconds"];
    ];

    (* Загрузка модулей *)
    loadPackage["Scattering.wl"];
    loadPackage["Plots/Plots.wl"];
    loadPackage["Plots/PlotStyle.wl"];
    loadPackage["Analytic/Analytic.wl"];
    loadPackage["Numeric/HarmonicOscillator.wl"];    
    loadPackage["Numeric/Numeric.wl"];
    loadPackage["CircuitTopology.wl"];
    loadPackage["Model.wl"];
    loadPackage["GUI/Interactive.wl"];

    debugPrint["QED initialization complete."];
];

End[];
EndPackage[];
