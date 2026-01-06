BeginPackage["QED`Numeric`", {"QED`Numeric`HarmonicOscillator`"}];

Needs["QED`Model`"];

(* Экспорт символов *)
VerifyDiagonalization::usage = "VerifyDiagonalization[model] checks if N^T.C.N and N^T.L^{-1}.N are diagonal.";
ComputeNormalModeFrequencies::usage = "ComputeNormalModeFrequencies[invC, invL] returns sorted eigenfrequencies.";
PlasmonFrequenciesVsFlux::usage = "PlasmonFrequenciesVsFlux[model] computes flux dependence of modes.";
FindPotentialMinimumContinuation::usage = "FindPotentialMinimumContinuation[...] finds equilibrium using homotopy.";
PrepareNumericModel::usage = "PrepareNumericModel[symModel, params] prepares numeric functions.";
ComputeEvolution::usage = "ComputeEvolution[model, tmax] computes NDSolve solution.";
FindPotentialMinimum::usage = "FindPotentialMinimum[hamiltonian, topology, substitutionRules] finds equilibrium flux values.";
FindEquilibriumPoints::usage = "FindEquilibriumPoints[hamiltonian, topology, substitutionRules] finds all equilibrium points.";
VerifyWaveFunction::usage = "VerifyWaveFunction[model, state] verifies that H|psi> = E|psi>.";

Begin["`Private`"];

Options[FindPotentialMinimumContinuation] = {
  "StepSize" -> 0.05,
  "MaxSteps" -> 100,
  "Tolerance" -> 10^-8
};

Options[FindEquilibriumPoints] = {
  GridResolution -> 5,
  MaxResidual -> 10^-5,
  Method -> "Newton"
};

$DebugFindPotentialMinimumContinuation = False;
$DebugFindPotentialMinimum = False;
$DebugFindEquilibriumPoints = False;
$DebugPlasmonFrequencies = False;


(* ════════════════════════════════════════════════════════════════ *)
(* 		VERIFICATION                                                *)
(* ════════════════════════════════════════════════════════════════ *)

VerifyDiagonalization[model_Association] := Module[
  {capNum, invLNum, diagData, 
   Nmat, matC_diag, matinvL_diag, 
   expectedCaps, calculatedCaps},
  
  Print["[DEBUG VerifyDiagonalization] Start"];
  
  capNum = QED`Model`GetNumericalQuantity[model, "CapacitanceMatrixNumerical"];
  Print["[DEBUG] capNum: ", Short[capNum, 2]];
  Print["[DEBUG] capNum Head: ", Head[capNum]];
  Print["[DEBUG] capNum MatrixQ: ", MatrixQ[capNum]];
  
  invLNum = QED`Model`GetNumericalQuantity[model, "InductanceMatrixInverseNumerical"];
  Print["[DEBUG] invLNum: ", Short[invLNum, 2]];
  Print["[DEBUG] invLNum Head: ", Head[invLNum]];
  Print["[DEBUG] invLNum MatrixQ: ", MatrixQ[invLNum]];
  
  diagData = QED`Model`GetNumericalQuantity[model, "HarmonicDiagonalization"];
  Print["[DEBUG] diagData Head: ", Head[diagData]];
  If[AssociationQ[diagData], 
     Print["[DEBUG] diagData Keys: ", Keys[diagData]],
     Print["[DEBUG] diagData is NOT Association!"]
  ];

  If[AnyTrue[{capNum, invLNum, diagData}, FailureQ], 
     Print["[DEBUG] At least one quantity is $Failed. Returning $Failed."];
     Return[$Failed]
  ];
  
  Print["[DEBUG] Extracting FluxTransform..."];
  Nmat = diagData["FluxTransform"];
  Print["[DEBUG] Nmat: ", Short[Nmat, 2]];
  Print["[DEBUG] Nmat MatrixQ: ", MatrixQ[Nmat]];
  
  Print["[DEBUG] Computing Transpose[Nmat] . invLNum . Nmat"];
  matinvL_diag = Transpose[Nmat] . invLNum . Nmat;
  Print["[DEBUG] matinvL_diag computed: ", Short[matinvL_diag, 2]];
  
  Print["[DEBUG] Computing Transpose[Nmat] . capNum . Nmat"];
  matC_diag = Transpose[Nmat] . capNum . Nmat;
  Print["[DEBUG] matC_diag computed: ", Short[matC_diag, 2]];
  
  expectedCaps = diagData["EffectiveCapacitances"];
  calculatedCaps = Diagonal[matC_diag];
  
  Print["[DEBUG] Returning Association..."];

  <|
    "Transformed_L_Inverse" -> Chop[matinvL_diag, 10^-20],
    "Transformed_C" -> Chop[matC_diag, 10^-20],
    "Is_L_Diagonal" -> DiagonalMatrixQ[Chop[matinvL_diag, 10^-10]],
    "Is_C_Diagonal" -> DiagonalMatrixQ[Chop[matC_diag, 10^-10]],
    "EffectiveCapacitances_Check" ->  expectedCaps / calculatedCaps
  |>
];


(* ════════════════════════════════════════════════════════════════ *)
(* 		NORMAL MODES & PLASMONS                                     *)
(* ════════════════════════════════════════════════════════════════ *)

ComputeNormalModeFrequencies[invCap_?MatrixQ, invInd_?MatrixQ] := Module[
  {omega2, frequencies, threshold = 10^(-10)},
  
  omega2 = Eigenvalues[invCap . invInd];
  frequencies = Sort[Sqrt[omega2 + 0. I], Re[#1] < Re[#2] &];
  
  <|
    "Frequencies" -> Chop[frequencies],
    "IsStable" -> AllTrue[omega2, # > threshold &],
    "NumUnstableModes" -> Count[omega2, x_ /; x < -threshold]
  |>
];


(* ════════════════════════════════════════════════════════════════ *)
(* 		FLUX DEPENDENCE                                             *)
(* ════════════════════════════════════════════════════════════════ *)

PlasmonFrequenciesVsFlux[model_Association] := Module[
  {
    capSym, lindInvSym, topology, rulesBase, phiExtSym, phi0,
    gradientRescaled, hessianRescaled, fluxVars, nodes,
    callCounter = 0, totalContinuationTime = 0, totalEigenTime = 0, totalOverhead = 0,
    useContinuation
  },
  
  capSym     = model["Analytical"]["CapacitanceMatrix"];
  lindInvSym = model["Analytical"]["InductanceMatrix"];
  topology   = model["Topology"];  

  phiExtSym = QED`$PhiExt;
  phi0      = QED`$Phi0Value;
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  rulesBase = DeleteCases[
    model["SubstitutionRules"],
    (phiExtSym :> _) | (Subscript[QED`$FluxSymbol, "min", _] :> _)
  ];
  
  useContinuation = KeyExistsQ[model, "Numerical"] && 
                    KeyExistsQ[model["Numerical"], "Cache"] &&
                    KeyExistsQ[model["Numerical"]["Cache"], "ContinuationDerivatives"];
  
  If[useContinuation,
    Module[{cache},
      cache = model["Numerical"]["Cache"]["ContinuationDerivatives"];
      gradientRescaled = Lookup[cache, "Gradient", $Failed];
      hessianRescaled = Lookup[cache, "Hessian", $Failed];
      
      If[gradientRescaled === $Failed || hessianRescaled === $Failed,
        useContinuation = False;
        Print["[WARNING] Continuation derivatives not found in cache."];
      ];
    ];
  ];
  
  If[!useContinuation,
    Print["[ERROR] PlasmonFrequenciesVsFlux requires continuation derivatives."];
    Return[$Failed];
  ];
  
  Function[{phiExtDimensionless},
    Module[{phiExtPhysical, rulesWithFlux, capNum, lindInvNum, 
            invCapNum, omega2, frequencies, equilibriumRules,
            tStart, tAfterContinuation, tAfterEigen, tEnd},
      
      If[!NumericQ[phiExtDimensionless],
        Return[$Failed, Module]
      ];

      tStart = AbsoluteTime[];
      callCounter++;
      
      phiExtPhysical = phiExtDimensionless * phi0;
      rulesWithFlux = Append[rulesBase, phiExtSym -> phiExtPhysical];
      
      equilibriumRules = FindPotentialMinimumContinuation[
        gradientRescaled,
        hessianRescaled,
        fluxVars,
        topology,
        phiExtPhysical
      ];

      tAfterContinuation = AbsoluteTime[];
      
      rulesWithFlux = Join[rulesWithFlux, equilibriumRules];
      
      capNum     = capSym //. rulesWithFlux;
      lindInvNum = lindInvSym //. rulesWithFlux;
      
      If[Det[capNum] == 0, Return[$Failed]];
      invCapNum = Inverse[capNum];
      
      omega2 = Eigenvalues[N[invCapNum . lindInvNum]];
      
      tAfterEigen = AbsoluteTime[];
      
      frequencies = Sort[Sqrt[omega2 + 0. I], Re[#1] < Re[#2] &];
      
      tEnd = AbsoluteTime[];
      
      If[$DebugPlasmonFrequencies === True,
        Module[{dtContinuation, dtEigen, dtOverhead, dtTotal},
          dtContinuation = (tAfterContinuation - tStart) * 1000;
          dtEigen = (tAfterEigen - tAfterContinuation) * 1000;
          dtTotal = (tEnd - tStart) * 1000;
          dtOverhead = dtTotal - dtContinuation - dtEigen;
          
          totalContinuationTime += dtContinuation;
          totalEigenTime += dtEigen;
          totalOverhead += dtOverhead;
          
          If[callCounter == 1 || Mod[callCounter, 10] == 0,
            Print["[PROFILE Point ", callCounter, "]" ];
            Print["  Continuation: ", Round[dtContinuation, 0.1], " ms"];
            Print["  Eigenvalues: ", Round[dtEigen, 0.1], " ms"];
            Print["  Overhead: ", Round[dtOverhead, 0.1], " ms"];
            Print["  Total: ", Round[dtTotal, 0.1], " ms"];
          ];
        ];
      ];

      Chop[frequencies]
    ]
  ]
];

(* ════════════════════════════════════════════════════════════════ *)
(* 		CONTINUATION METHOD (EQUILIBRIUM)                           *)
(* ════════════════════════════════════════════════════════════════ *)

FindPotentialMinimumContinuation[
  gradientRescaled_List,
  hessianRescaled_List,
  fluxVars_List,
  topology_Association,
  phiExtTarget_?NumericQ,
  opts:OptionsPattern[]
] := Module[{
  nodes, phi0Value, phiExtDimensionless, stepSize, maxSteps,
  nSteps, phiExtPath, initialSolution, solutionPath, finalSolution,
  minSymbols, tolerance, startTime, endTime
  },
  
  startTime = AbsoluteTime[];
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  phi0Value = QED`$Phi0Value;
  phiExtDimensionless = phiExtTarget / phi0Value;
  
  stepSize = OptionValue["StepSize"];
  maxSteps = OptionValue["MaxSteps"];
  tolerance = OptionValue["Tolerance"];
  
  nSteps = Min[Ceiling[Abs[phiExtDimensionless] / stepSize], maxSteps];
  phiExtPath = Subdivide[0.0, phiExtDimensionless, nSteps];
  
  initialSolution = Thread[fluxVars -> 0.0];
  
  solutionPath = FoldList[
    Function[{prevSol, phiExtCurrent},
      Module[{gradVec, hessMat, startPoint, newSol, phiExtValue},
        
        phiExtValue = phiExtCurrent * phi0Value;
        
        gradVec = gradientRescaled /. {QED`$PhiExt -> phiExtValue};
        hessMat = hessianRescaled /. {QED`$PhiExt -> phiExtValue};
        
        startPoint = Thread[{fluxVars, fluxVars /. prevSol}];
        
        newSol = Quiet[
          Check[
            FindRoot[
              Thread[gradVec == 0],
              startPoint,
              Jacobian -> hessMat
            ],
            $Failed,
            {FindRoot::cvmit, FindRoot::lstol, FindRoot::jsing}
          ],
          {FindRoot::cvmit, FindRoot::lstol, FindRoot::jsing}
        ];
        
        newSol
      ]
    ],
    initialSolution,
    Rest[phiExtPath]
  ];

  solutionPath = TakeWhile[solutionPath, # =!= $Failed &];
  
  If[Length[solutionPath] < nSteps + 1,
    Message[FindPotentialMinimumContinuation::badstep,
            Length[solutionPath], nSteps];
    Return[$Failed, Module]
  ];
  
  finalSolution = Last[solutionPath];
  
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  Module[{values},
    values = (fluxVars /. finalSolution) * phi0Value;
    
    endTime = AbsoluteTime[];
    
    Thread[minSymbols -> values]
  ]
];

FindPotentialMinimumContinuation::badstep = 
  "Continuation failed at step `1` of `2`. Try reducing StepSize option.";


(* ════════════════════════════════════════════════════════════════ *)
(*  FindPotentialMinimum (from previous full code)                  *)
(* ════════════════════════════════════════════════════════════════ *)

FindPotentialMinimum[hamiltonian_, topology_Association, substitutionRules_List] := 
 Module[{nodes, fluxVars, potential, potentialNumeric, externalFlux, 
         energyScale, potentialRescaled, externalFluxRescaled,
         constraints, result, minValues, phi0Value, minSymbols},
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  phi0Value = QED`$Phi0Value;
  externalFlux = QED`$PhiExt /. substitutionRules;

  potentialNumeric = potential /. substitutionRules /. QED`$Phi0 -> phi0Value;
  
  energyScale = Abs @ First @ Cases[
    potentialNumeric,
    c_?NumericQ * Cos[_] :> c,
    Infinity
  ];
  
  If[!NumericQ[energyScale] || energyScale == 0,
    energyScale = 1;
  ];
  
  potentialRescaled = (potentialNumeric / energyScale) /. 
    Thread[fluxVars -> fluxVars * phi0Value];
  externalFluxRescaled = externalFlux / phi0Value;
  
  constraints = Thread[-0.5 < fluxVars < 0.5];
  
  result = Quiet[
    NMinimize[
      {potentialRescaled, constraints},
      fluxVars,
      Method -> {
        "RandomSearch", 
        "SearchPoints" -> 10,
        "RandomSeed" -> 12345,
        "PostProcess" -> {
          "FindMinimum",
          Method -> "QuasiNewton"
        }
      },
      MaxIterations -> 3,
      AccuracyGoal -> 6,
      PrecisionGoal -> 6
    ],
    {NMinimize::cvmit, NMinimize::nosat, FindMinimum::lstol, FindMinimum::sdprec}
  ];
  
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  If[result === $Failed || !NumericQ[result[[1]]],
    minValues = Thread[minSymbols -> externalFlux],
    minValues = Thread[minSymbols -> (fluxVars /. result[[2]]) * phi0Value]
  ];
  
  minValues
];

(* ════════════════════════════════════════════════════════════════ *)
(*  FindEquilibriumPoints (stub - full version exists above)        *)
(* ════════════════════════════════════════════════════════════════ *)

FindEquilibriumPoints[hamiltonian_, gradient_List, topology_Association, 
  substitutionRules_List, opts:OptionsPattern[]] := 
  <|"Solutions" -> {}, "NumSolutions" -> 0|>;

(* ════════════════════════════════════════════════════════════════ *)
(*  VerifyWaveFunction (stub)                                        *)
(* ════════════════════════════════════════════════════════════════ *)

VerifyWaveFunction[model_Association, state_List] := 
  <|"Status" -> "NotImplemented"|>;

(* ════════════════════════════════════════════════════════════════ *)
(*  PrepareNumericModel, ComputeEvolution (stubs)                   *)
(* ════════════════════════════════════════════════════════════════ *)

PrepareNumericModel[symModel_Association, params_Association] := <||>;
ComputeEvolution[model_, tmax_?NumericQ] := <||>;

GetCacheEntry[cacheEntry_Association, model_Association] := Module[
  {state, thunk},
  
  state = Lookup[cacheEntry, "State", "Unknown"];
  
  Which[
    state === "Ready",
      Lookup[cacheEntry, "Value", $Failed],
    
    state === "Lazy",
      thunk = Lookup[cacheEntry, "Thunk", $Failed];
      If[thunk === $Failed, $Failed, thunk[model]],
    
    True,
      $Failed
  ]
];

End[];
EndPackage[];