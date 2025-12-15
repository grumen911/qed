BeginPackage["QED`Analytic`"];

BuildLagrangian::usage = "BuildLagrangian[topology, primaryParams] builds symbolic Lagrangian.";
BuildCapacitanceMatrix::usage = "BuildCapacitanceMatrix[lagrangian, topology] builds symbolic capacitance matrix.";
BuildHamiltonian::usage = "BuildHamiltonian[lagrangian, capMatrix, topology] builds symbolic Hamiltonian.";

Begin["`Private`"];


(* Используем глобальные константы из QED` *)
phi0 = QED`$Phi0;  


(* Вспомогательная функция для получения независимых узлов *)
getIndependentNodes[topology_Association] := 
  Cases[topology["Nodes"], Except[topology["GroundNode"]]];


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
 Module[{components, terms, groundNode, fluxLoops},
  
  components = topology["Components"];
  groundNode = topology["GroundNode"];
  fluxLoops = topology["GraphStructure"]["fluxLoops"];
    
  terms = Map[
    Module[{type, n1, n2, tag, symbols, flux, fluxExt, fluxTotal,
    		 	fluxDot, params, loopData}, 
      {type, n1, n2, tag, symbols} = PadRight[#, 5, <||>];
      
      flux = Subscript[QED`$FluxSymbol, n1] - Subscript[QED`$FluxSymbol, n2];
      
      (* Проверить, является ли этот компонент хордой петли *)
      loopData = Lookup[fluxLoops, tag, Missing[]];
      fluxExt = If[MissingQ[loopData], 
        0,
        loopData["ExternalFluxSymbol"]
      ];
      
      (* Полный поток *)
      fluxTotal = flux + fluxExt;
      
      fluxDot = flux /. Subscript[QED`$FluxSymbol, n_] :> 
      					Derivative[1][Subscript[QED`$FluxSymbol, n]][t];
      
      params = primaryParams[tag];
      
      Switch[type,
        "Capacitor",
        params["C"]["Symbol"]/2 * fluxDot^2,
        
        "Inductor",
        -fluxTotal^2/(2 * params["L"]["Symbol"]),
        
        "JosephsonJunction",
        params["CJ"]["Symbol"]/2 * fluxDot^2 + 
          params["EJ"]["Symbol"] * Cos[2 Pi fluxTotal / phi0],
        
        _, 0
      ]
    ] &,
    components
  ];
  
  Simplify[Total[terms] /. Subscript[QED`$FluxSymbol, groundNode] -> 0]
 ];


BuildCapacitanceMatrix[lagrangian_, topology_Association] := 
 Module[{nodes, phiDotVars, capacitanceMatrix},
  nodes = getIndependentNodes[topology];
  phiDotVars = Derivative[1][Subscript[QED`$FluxSymbol, #]][t] & /@ nodes;
  capacitanceMatrix = Outer[
    D[D[lagrangian, #1], #2] &,
    phiDotVars,
    phiDotVars
  ];
  Simplify[capacitanceMatrix]
 ];


BuildHamiltonian[lagrangian_, capMatrix_, topology_Association] := 
 Module[{nodes, phiVars, phiDotVars, qVars, kineticEnergy, potentialEnergy},
  nodes = getIndependentNodes[topology];
  phiVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  phiDotVars = Derivative[1][Subscript[QED`$FluxSymbol, #]][t] & /@ nodes;
  qVars = Subscript[QED`$ChargeSymbol, #] & /@ nodes;
  
  kineticEnergy = (1/2) * qVars . Inverse[capMatrix] . qVars;
  potentialEnergy = -lagrangian /. Thread[phiDotVars -> 0];
  
  (*Долгая операция*)
  Simplify[kineticEnergy + potentialEnergy]
 ];


End[];
EndPackage[];