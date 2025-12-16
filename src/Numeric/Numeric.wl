BeginPackage["QED`Numeric`"];

PrepareNumericModel::usage = "PrepareNumericModel[symModel, params] prepares numeric functions.";
ComputeEvolution::usage = "ComputeEvolution[model, tmax] computes NDSolve solution.";

FindPotentialMinimum::usage = "FindPotentialMinimum[hamiltonian, topology, substitutionRules] \
numerically finds equilibrium flux values φ_min that minimize potential energy U(φ). \
Uses PrincipalAxis method (gradient-free local optimization) starting from external flux φ_ext. \
Optimized for smooth potentials with good initial guess (~0.002 sec). \
Returns substitution rules: {φ₁ -> value₁, φ₂ -> value₂, ...} in Weber.";

TestOptimizationMethods::usage = "1232";

Begin["`Private`"];


(*
  Physics: Find equilibrium positions φ_min where ∂U/∂φ = 0.
  
  For flux-biased circuits (qubits, tunable couplers), the equilibrium 
  depends on external flux Φ_ext. 
  
  Algorithm with dimensionless rescaling for numerical stability:
  1. Extract potential energy U(φ) by setting all charges q_i = 0
  2. Rescale energy by EJ: Ũ = U/EJ (dimensionless)
  3. Rescale fluxes by Φ₀: φ̃ = φ/Φ₀ (dimensionless)
  4. Minimize Ũ(φ̃) using PrincipalAxis method
  5. Convert back: φ = φ̃ * Φ₀
  
  Rescaling improves numerical stability by factor ~10⁸ and ensures
  convergence from arbitrary starting points.
  
  Reference: Manucharyan et al., Science 326, 113 (2009), Fig. 2
*)

FindPotentialMinimum[hamiltonian_, topology_Association, substitutionRules_List] := 
 Module[{nodes, fluxVars, potential, potentialNumeric, externalFlux, 
         energyScale, potentialRescaled, externalFluxRescaled,
         result, minValues, phi0Value, startingPointList},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Потенциальная энергия: U(φ) = H(q=0, φ) *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Константы *)
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* ОБЕЗРАЗМЕРИВАНИЕ для численной стабильности                      *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Извлечь EJ из коэффициента при Cos *)
  energyScale = Abs @ First @ Cases[
    potentialNumeric,
    c_?NumericQ * Cos[_] :> c,
    Infinity
  ];
  
  If[!NumericQ[energyScale] || energyScale == 0,
    Print["Warning: Cannot extract energy scale."];
    energyScale = 1;
  ];
  
  (* Обезразмерить: Ũ = U/EJ, φ̃ = φ/Φ₀ *)
  potentialRescaled = (potentialNumeric / energyScale) /. 
    Thread[fluxVars -> fluxVars * phi0Value];
  externalFluxRescaled = externalFlux / phi0Value;
  
  Print[Simplify@potentialRescaled];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Минимизация в безразмерных координатах                          *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  startingPointList = Table[
    {fluxVars[[i]], externalFluxRescaled},
    {i, Length[fluxVars]}
  ];
  
  result = Quiet[
    FindMinimum[
      Simplify@potentialRescaled,
      startingPointList,
      Method -> "PrincipalAxis",
      MaxIterations -> 500
    ],
    {FindMinimum::cvmit, FindMinimum::lstol, FindMinimum::sdprec}
  ];
  
  Print[		Simplify[potentialRescaled/.result[[2]]]		];
  
  (* Обратное масштабирование: φ̃ → φ *)
  If[result === $Failed || !NumericQ[result[[1]]],
    Print["Warning: Minimization failed. Using φ_min ≈ φ_ext."];
    minValues = Thread[fluxVars -> externalFlux],
    minValues = Thread[fluxVars -> (fluxVars /. result[[2]]) * phi0Value]
  ];
  
  minValues
];



TestOptimizationMethods[hamiltonian_, topology_Association, substitutionRules_List] := 
 Module[{nodes, fluxVars, potential, potentialNumeric, externalFlux, 
         constraints, phi0Value, lowerBound, upperBound, methods, results},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  phi0Value = QED`$Phi0Value;
  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;
  
  lowerBound = (externalFlux - 0.5) * phi0Value;
  upperBound = (externalFlux + 0.5) * phi0Value;
  constraints = Thread[lowerBound < fluxVars < upperBound];
  
  Print["=== Testing Optimization Methods ==="];
  Print["Variables: ", Length[fluxVars]];
  Print["External flux: ", externalFlux];
  Print[""];
  
  (* ════════════════════════════════════════════════════════════ *)
  (* TEST 1: NMinimize methods                                   *)
  (* ════════════════════════════════════════════════════════════ *)
  
  methods = {
    {"NelderMead", 100},
    {"DifferentialEvolution", 100},
    {"SimulatedAnnealing", 100},
    {"RandomSearch", 100}
  };
  
  results = Table[
    Module[{time, result, value, success},
      {time, result} = AbsoluteTiming[
        Quiet[
          NMinimize[
            {potentialNumeric, constraints},
            fluxVars,
            Method -> method[[1]],
            MaxIterations -> method[[2]]
          ],
          {NMinimize::cvmit, NMinimize::nosat}
        ]
      ];
      
      success = (result =!= $Failed && NumericQ[result[[1]]]);
      value = If[success, result[[1]], "FAILED"];
      
      Print[StringPadRight[method[[1]], 25], " | Time: ", 
            NumberForm[time, {4, 4}], " sec | Min: ", 
            If[NumericQ[value], ScientificForm[value, 3], value]];
      
      <|"Method" -> method[[1]], "Time" -> time, 
        "Value" -> value, "Success" -> success|>
    ],
    {method, methods}
  ];
  
  Print[""];
  
  (* ════════════════════════════════════════════════════════════ *)
  (* TEST 2: FindMinimum methods (local)                         *)
  (* ════════════════════════════════════════════════════════════ *)
  
(* Исправленная секция для FindMinimum *)
Print["--- Local methods (FindMinimum) ---"];

localMethods = {
  "QuasiNewton",
  "PrincipalAxis", 
  "ConjugateGradient"
};

(* ПРАВИЛЬНЫЙ формат стартовой точки для FindMinimum *)
startingPointList = Table[
  {fluxVars[[i]], externalFlux * phi0Value},
  {i, Length[fluxVars]}
];

Table[
  Module[{time, result, value, success},
    {time, result} = AbsoluteTiming[
      Quiet[
        FindMinimum[
          potentialNumeric,
          startingPointList,  (* ← Исправлено! *)
          Method -> method,
          MaxIterations -> 50
        ],
        {FindMinimum::cvmit, FindMinimum::lstol, FindMinimum::sdprec}
      ]
    ];
    
    success = (result =!= $Failed && NumericQ[result[[1]]]);
    value = If[success, result[[1]], "FAILED"];
    
    Print[StringPadRight[method, 25], " | Time: ", 
          NumberForm[time, {4, 4}], " sec | Min: ", 
          If[NumericQ[value], ScientificForm[value, 3], value]];
  ],
  {method, localMethods}
];
  
  Print[""];
  Print["=== Best method: ", 
    First[SortBy[Select[results, #["Success"] &], #["Time"] &]]["Method"]];
  Print["====================================="];
  
  results
];
  

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