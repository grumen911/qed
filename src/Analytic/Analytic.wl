BeginPackage["QED`Analytic`"];

BuildLagrangian::usage = "BuildLagrangian[topology, primaryParams] builds symbolic Lagrangian.";
BuildCapacitanceMatrix::usage = "BuildCapacitanceMatrix[lagrangian, topology] builds symbolic capacitance matrix.";
BuildHamiltonian::usage = "BuildHamiltonian[lagrangian, capMatrix, topology] builds symbolic Hamiltonian.";
BuildHarmonicHamiltonian::usage = "BuildHarmonicHamiltonian[hamiltonian, topology] expands the Hamiltonian to second order around the potential minimum \[Phi]_min.";


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
  capacitanceMatrix
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
  Collect[kineticEnergy + potentialEnergy, Join[phiVars, qVars], Simplify]
 ];


(* ════════════════════════════════════════════════════════════════ *)
(*                 HARMONIC APPROXIMATION                           *)
(* ════════════════════════════════════════════════════════════════ *)

(*
  Physics: Expand Hamiltonian to quadratic order around equilibrium.
  
  H(φ) ≈ H(φ_min) + (1/2) ∑ᵢⱼ Kᵢⱼ (φᵢ - φᵢ,min)(φⱼ - φⱼ,min)
  
  where Kᵢⱼ = ∂²H/∂φᵢ∂φⱼ|_min is the Hessian matrix.
  
  For Josephson junctions:
  -E_J Cos[2πφ/Φ₀] ≈ -E_J + E_J(π/Φ₀)²(φ - φ_min)²
  
  Reference: Koch et al., PRA 76, 042319 (2007), Eq. (6-8)
*)

BuildHarmonicHamiltonian[hamiltonian_, topology_Association] := 
 Module[{nodes, fluxVars, chargeVars, minSymbols, series, degree, result},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  chargeVars = Subscript[QED`$ChargeSymbol, #] & /@ nodes;
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Series expansion to O(φ²) around φ_min *)
  series = Normal @ Series[
    hamiltonian,
    Sequence @@ MapThread[{#1, #2, 2} &, {fluxVars, minSymbols}]
  ] // Expand;
  
  (* Helper: total polynomial degree in flux variables *)
  degree[term_] := Total @ Exponent[term, fluxVars];
  
  (* Keep only constant (degree 0) and quadratic (degree 2) terms *)
  result = Total @ Cases[
    If[Head[series] === Plus, List @@ series, {series}],
    term_ /; degree[term] == 0 || degree[term] == 2
  ];
  
  (* Collect by physical variables for readability, simplify coefficients *)
  Collect[result, Join[fluxVars, chargeVars], Simplify]
 ];
 

End[];
EndPackage[];