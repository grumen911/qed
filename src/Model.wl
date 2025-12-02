BeginPackage["QED`Model`"];

GetSymbolic::usage = "GetSymbolic[model, key] extracts symbolic form.";
GetNumeric::usage = "GetNumeric[model, key] extracts numeric form.";

Begin["`Private`"];

CreateModel[params_Association] := Module[{sym, num},
  sym = <|
    "H" -> QED`Analytic`BuildHamiltonian[params],
    "Energies" -> QED`Analytic`DiagonalizeSymbolic[params],
    "Operators" -> params
  |>;
  num = QED`Numeric`PrepareNumericModel[sym, params];
  <|
    "Parameters" -> params,
    "Symbolic" -> sym,
    "Numeric" -> num
  |>
];

GetSymbolic[model_, key_] := model["Symbolic"][key];
GetNumeric[model_, key_]  := model["Numeric"][key];

End[];
EndPackage[];