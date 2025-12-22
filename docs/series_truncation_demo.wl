(* ::Package:: *)

pd = 2;

pstr[e_] := ToString[
  N[SetPrecision[e, pd], pd],
  InputForm,
  NumberMarks -> False
];



ϕ0 = 1.0;
Foff = 1.7;

U[p1_, p2_] := -Cos[(p1 - p2 + Foff)/ϕ0] - Cos[p1/ϕ0];

dom = {-Pi <= p1 <= Pi, -Pi <= p2 <= Pi};

nPerVar = 2;
nTotalDeg = 2;

sol = NMinimize[{U[p1, p2], And @@ dom}, {p1, p2}, Method -> "DifferentialEvolution"];  (* [web:299] *)

Umin = sol[[1]];
rulesMin = sol[[2]];
p1m = p1 /. rulesMin;
p2m = p2 /. rulesMin;


gradAtMin = {D[U[p1, p2], p1], D[U[p1, p2], p2]} /. rulesMin // Chop;


serPerVarExpr =
  Normal@Series[U[x, y], {x, p1m, nPerVar}, {y, p2m, nPerVar}] /. {x -> p1, y -> p2} // Expand;

serPerVarExprShadow2 =
  FromCoefficientRules[
    Select[CoefficientRules[serPerVarExpr, {p1, p2}], Total[First[#]] <= 2 &],
    {p1, p2}
  ] // Expand;


serTotalDegExpr =
 Module[{d1 = p1 - p1m, d2 = p2 - p2m},
  Normal@Series[U[p1m + t d1, p2m + t d2], {t, 0, nTotalDeg}] /. t -> 1 // Expand
 ];

diff = Chop[serPerVarExprShadow2 - serTotalDegExpr] // Expand;


Print["=== Multivariate Series truncation demo (WL) ==="];

Print["U(p1,p2) = -Cos[(p1 - p2 + Foff)/ϕ0] - Cos[p1/ϕ0]"];

Print["params: ϕ0=", pstr[ϕ0], "  Foff=", pstr[Foff]];
Print["Umin = ", pstr[Umin]];
Print["argmin (p1m,p2m) = (", pstr[p1m], ", ", pstr[p2m], ")"];
Print["grad at argmin = {", pstr[gradAtMin[[1]]], ", ", pstr[gradAtMin[[2]]], "}"];

Print["--- Series per-variable ---"];
Print[pstr[serPerVarExpr]];

Print["--- Series per-variable (keep only degree<=2 in {p1,p2}; 'shadows') ---"];
Print[pstr[serPerVarExprShadow2]];

Print["--- Series total-degree (via t) ---"];
Print[pstr[serTotalDegExpr]];

Print["--- Difference (per-variable (with shadows) - total-degree) ---"];
Print[pstr[diff]];


Print["Done."];
