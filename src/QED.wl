BeginPackage["QED`"];

(* публичные функции *)
ComputeEvolution::usage = "ComputeEvolution[model, tmax] computes numerical evolution.";
MyInteractiveModule::usage = "MyInteractiveModule[initParams] creates interactive UI.";
MyCompareModule::usage  = "MyCompareModule[p1, p2] compares two parameter sets.";

Begin["`Private`"];

QED`Bootstrap`Initialize[];

End[];
EndPackage[];