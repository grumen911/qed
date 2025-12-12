BeginPackage["QED`"];

Unprotect["QED`*"];   (* Снимаем защиту *)
ClearAll["QED`*"];    (* Полностью очищаем определения и значения *)

(* публичные функции *)
MyInteractiveModule::usage = "MyInteractiveModule[initParams] creates interactive UI.";
MyCompareModule::usage  = "MyCompareModule[p1, p2] compares two parameter sets.";

Begin["`Private`"];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Bootstrap.wl"}]];
QED`Bootstrap`InitQED[];

End[];
EndPackage[];