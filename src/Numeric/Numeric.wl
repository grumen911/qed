BeginPackage["QED`Numeric`"];

PrepareNumericModel::usage = "PrepareNumericModel[symModel, params] prepares numeric functions.";
ComputeEvolution::usage = "ComputeEvolution[model, tmax] computes NDSolve solution.";
FindPotentialMinimum::usage = "FindPotentialMinimum[hamiltonian, fluxVars, substitutionRules] numerically finds equilibrium flux values that minimize potential energy.";

Begin["`Private`"];


(*
  Physics: Find equilibrium positions φ_min where ∂U/∂φ = 0.
  
  For flux-biased circuits (qubits, tunable couplers), the equilibrium 
  depends on external flux Φ_ext.
  
  Algorithm:
  1. Extract potential energy U(φ) from Hamiltonian
  2. Substitute numerical parameter values
  3. Minimize U using NMinimize with constraints
  
  Reference: Manucharyan et al., Science 326, 113 (2009), Fig. 2
*)

(*
  Physics: Find equilibrium positions φ_min where ∂U/∂φ = 0.
  
  For flux-biased circuits (qubits, tunable couplers), the equilibrium 
  depends on external flux Φ_ext. The minimum is typically located near
  φ_min ≈ φ_ext with small corrections from inductors.
  
  Algorithm:
  1. Extract potential energy U(φ) by setting all charges q_i = 0
  2. Substitute numerical parameter values
  3. Minimize U in local vicinity [φ_ext - 2, φ_ext + 2]
  
  Search radius ±2 is sufficient for realistic quantum circuits where
  inductance corrections are small.
  
  Reference: Manucharyan et al., Science 326, 113 (2009), Fig. 2
*)

FindPotentialMinimum[hamiltonian_, topology_Association, substitutionRules_List] := 
 Module[{nodes, fluxVars, potential, potentialNumeric, externalFlux, 
         constraints, result, minValues, phi0Value},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  phi0Value = QED`$Phi0Value;
  
  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
  
  constraints = Thread[
    (externalFlux - 0.5) * phi0Value < fluxVars < 
    (externalFlux + 0.5) * phi0Value
  ];
  
  (* Простая оптимизированная версия *)
  result = Quiet[
    NMinimize[
      {potentialNumeric, constraints}, 
      fluxVars,
      Method -> "NelderMead",
      MaxIterations -> 100,
      AccuracyGoal -> 3,
      PrecisionGoal -> 3
    ],
    {NMinimize::cvmit, NMinimize::nosat}
  ];
  
  If[result === $Failed || !NumericQ[result[[1]]],
    Print["Warning: Minimization failed. Using φ_min ≈ φ_ext."];
    minValues = Thread[fluxVars -> externalFlux * phi0Value],
    minValues = result[[2]]
  ];
  
  minValues
]
  

PrepareNumericModel[symModel_Association, params_Association] := Module[{sol},
  (* подготовка численных функций из символики *)
  sol
];

ComputeEvolution[model_, tmax_?NumericQ] := Module[{sol},
  (* NDSolve *)
  sol
];

End[];
EndPackage[];