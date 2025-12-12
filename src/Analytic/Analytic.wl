BeginPackage["QED`Analytic`"];

BuildLagrangian::usage = "BuildLagrangian[params] builds symbolic Lagrangian.";
BuildCapacitanceMatrix::usage = "BuildCapacitanceMatrix[params] builds symbolic Capacitance Matrix.";
BuildHamiltonian::usage = "BuildHamiltonian[params] builds symbolic Hamiltonian.";
DiagonalizeSymbolic::usage = "DiagonalizeSymbolic[H] diagonalizes symbolic Hamiltonian.";

Begin["`Private`"];


(* Константа для магнитного потока *)
phiZero = Subscript[\[CapitalPhi], 0];


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


BuildLagrangian[topology_Association, primaryParams_Association] := 
 Module[{components, terms},
  
  components = topology["Components"];
  
  terms = Map[
    Module[{type, n1, n2, tag, symbols, flux, fluxDot, params}, 
      {type, n1, n2, tag, symbols} = PadRight[#, 5, <||>];
      flux = Subscript[\[Phi], n1] - Subscript[\[Phi], n2];
      fluxDot = flux /. Subscript[\[Phi], n_] :> Derivative[1][Subscript[\[Phi], n]][t];
      params = primaryParams[tag];
      
      Switch[type,
        "Capacitor",
        params["C"]["Symbol"]/2 * fluxDot^2,
        
        "Inductor",
        -flux^2/(2 * params["L"]["Symbol"]),
        
        "JosephsonJunction",
        params["CJ"]["Symbol"]/2 * fluxDot^2 + 
          params["EJ"]["Symbol"] * Cos[2 Pi flux / phiZero],
        
        _, 0
      ]
    ] &,
    components
  ];
  
  Total[terms]
 ];


BuildCapacitanceMatrix[lagrangian_, nodeList_List] := 
 Module[{phiDotVars, capacitanceMatrix},
  
  (* Список производных узловых потоков *)
  phiDotVars = Derivative[1][Subscript[\[Phi], #]][t] & /@ nodeList;
  
  (* Вычисляем матрицу: C_ij = ∂²L/(∂φ̇ᵢ ∂φ̇ⱼ) *)
  capacitanceMatrix = Outer[
    D[D[lagrangian, #1], #2] &,
    phiDotVars,
    phiDotVars
  ];
  
  Simplify[capacitanceMatrix]
 ]


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