BeginPackage["QED`Analytic`"];

BuildHamiltonian::usage = "BuildHamiltonian[params] builds symbolic Hamiltonian.";
DiagonalizeSymbolic::usage = "DiagonalizeSymbolic[H] diagonalizes symbolic Hamiltonian.";

Begin["`Private`"];


(* Константа для магнитного потока *)
$PhiZero = 2.067833848*^-15;  (* Φ₀ = h/(2e) в Вб *)


(* ════════════════════════════════════════════════════════════════ *)
(* ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ РАБОТЫ С SUBSCRIPT                   *)
(* ════════════════════════════════════════════════════════════════ *)

(* Преобразовать Subscript в плоские переменные для Integrate и т.д. *)
FlattenSubscripts[expr_] := 
  expr /. Subscript[sym_, idx_] :> Symbol[ToString[sym] <> ToString[idx]];

(* Обратное преобразование *)
UnflattenSubscripts[expr_, baseSymbol_] := 
  expr /. s_Symbol :> 
    With[{name = SymbolName[s]},
      If[StringStartsQ[name, ToString[baseSymbol]],
        Subscript[baseSymbol, ToExpression[StringDrop[name, StringLength[ToString[baseSymbol]]]]],
        s
      ]
    ];

(* Конвертация в LaTeX для MaTeX *)
ToCustomTeX[expr_] := Module[{tex},
  tex = ToString[expr, TeXForm];
  tex = StringReplace[tex, {
    "\\phi _{" ~~ n__ ~~ "}" :> "\\phi_{" <> n <> "}",
    "\\text{nodeFlux}_{" ~~ n__ ~~ "}" :> "\\phi_{" <> n <> "}"
  }];
  tex
];


BuildHamiltonian[params_Association] := Module[{sol},
  (* аналитика *)
  sol
];

DiagonalizeSymbolic[H_] := Module[{sol},
  (* символьная диагонализация *)
  sol
];

End[];
EndPackage[];