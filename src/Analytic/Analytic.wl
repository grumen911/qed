BeginPackage["QED`Analytic`"];

BuildLagrangian::usage = "BuildLagrangian[topology, primaryParams] builds symbolic Lagrangian.";
BuildCapacitanceMatrix::usage = "BuildCapacitanceMatrix[lagrangian, topology] builds symbolic capacitance matrix.";
BuildHamiltonian::usage = "BuildHamiltonian[lagrangian, capMatrix, topology] builds symbolic Hamiltonian.";
BuildHarmonicHamiltonian::usage = "BuildHarmonicHamiltonian[hamiltonian, topology] expands the Hamiltonian to second order around the potential minimum \[Phi]_min.";
BuildInductanceMatrix::usage = "BuildInductanceMatrix[lagrangian, topology] builds symbolic inductance matrix.";

BuildPotentialGradient::usage = "BuildPotentialGradient[hamiltonian, topology] \
builds symbolic gradient ∇U of potential energy U(φ) = H(q=0, φ). \
Returns list of partial derivatives {∂U/∂φ₁, ∂U/∂φ₂, ...} for equilibrium analysis.";

BuildHarmonicWavefunction::usage = 
"BuildHarmonicWavefunction[topology, frequencies, effectiveCapacitances, projectionMatrix, quantumNumbers] \
constructs the analytical harmonic oscillator wavefunction in the original flux coordinates.

Arguments:
  topology: Association describing the circuit topology
  frequencies: List of normal mode frequencies {ω₁, ω₂, ...}
  effectiveCapacitances: List of effective capacitances {C₁, C₂, ...} for each mode
  projectionMatrix: Matrix K such that q_normal = K · (φ - φ_min). 
                    (Note: This is the inverse of the mode shape matrix).
  quantumNumbers: List of integers {n₁, n₂, ...} specifying the state

Returns:
  Symbolic expression Ψ(φ₁, φ₂, ...). Includes Jacobian normalization factor Sqrt[Det[K]].";

Begin["`Private`"];


(* Используем глобальные константы из QED` *)
phi0 = QED`$Phi0;  
hbar = QED`$hbar;


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
      fluxDot = D[fluxTotal, QED`$TimeSymbol];
      
      params = Lookup[primaryParams, name, <||>];
      
      (* Лагранжиан компонента *)
      Switch[type,
        "Capacitor", 
          0.5 * symbols["C"] * fluxDot^2,
          
        "Inductor", 
          -0.5 * (1/symbols["L"]) * fluxTotal^2,
          
        "JosephsonJunction", 
          symbols["EJ"] * Cos[2*Pi*fluxTotal/QED`$Phi0],
          
        _, 0
      ]
    ] &,
    components
  ];
  
  Total[terms]
];


BuildCapacitanceMatrix[lagrangian_, topology_] := 
 Module[{nodes, fluxVars, fluxDots, n, capMatrix},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  fluxDots = D[fluxVars, QED`$TimeSymbol];
  n = Length[nodes];
  
  (* C_ij = ∂²L / ∂φ̇_i ∂φ̇_j *)
  capMatrix = Table[
    D[lagrangian, fluxDots[[i]], fluxDots[[j]]],
    {i, n}, {j, n}
  ];
  
  capMatrix
];


BuildHamiltonian[lagrangian_, capMatrix_, topology_] := 
 Module[{nodes, fluxVars, chargeVars, n, kineticEnergy, potentialEnergy},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  chargeVars = Subscript[QED`$ChargeSymbol, #] & /@ nodes;
  n = Length[nodes];
  
  (* Кинетическая энергия T = 0.5 * q^T * C^-1 * q *)
  kineticEnergy = 0.5 * chargeVars . Inverse[capMatrix] . chargeVars;
  
  (* Потенциальная энергия U = -L + T(φ̇ -> 0) *)
  (* Внимание: L содержит T - U. Значит U = T - L. *)
  (* Но T записана через φ̇. Нам нужно выражение от φ. *)
  (* Для стандартных лагранжианов L = T(φ̇) - U(φ). *)
  (* Значит U(φ) = - (L /. φ̇ -> 0) *)
  
  potentialEnergy = -(lagrangian /. D[_, QED`$TimeSymbol] -> 0);
  
  kineticEnergy + potentialEnergy
];


BuildHarmonicHamiltonian[hamiltonian_, topology_] := 
 Module[{nodes, fluxVars, minFluxVars, potential, potentialExp, 
         kinetic, hHarmonic, deltaPhi},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minFluxVars = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Разделить гамильтониан *)
  kinetic = hamiltonian /. _Subscript[QED`$FluxSymbol, _] -> 0; (* Приближенно *)
  (* Точнее: выделить слагаемые с зарядом *)
  kinetic = Select[hamiltonian, !FreeQ[#, QED`$ChargeSymbol] &];
  potential = hamiltonian - kinetic;
  
  (* Разложение потенциала вокруг минимума *)
  deltaPhi = fluxVars - minFluxVars;
  
  (* U ≈ U(φ_min) + 0.5 * (φ-φ_min)^T * H * (φ-φ_min) *)
  (* Мы опускаем константу U(φ_min) для гармонического гамильтониана, 
     но для спектра она важна. Добавим её? Обычно H_osc отсчитывают от дна. *)
     
  (* Вычисляем Гессиан в точке минимума (символьно) *)
  (* Hessian_ij = ∂²U / ∂φ_i ∂φ_j *)
  
  (* Для символьного разложения просто берем 2-й порядок Series *)
  (* Но Series по многим переменным сложен. Проще градиент и гессиан. *)
  
  (* Делаем замену φ -> φ_min + δφ и разлагаем по δφ до 2 порядка *)
  potentialExp = Normal[Series[
    potential /. Thread[fluxVars -> minFluxVars + deltaPhi],
    {deltaPhi[[1]], 0, 2}, {deltaPhi[[2]], 0, 2} (* TODO: Generalize for N vars *)
  ]];
  
  (* Оставляем только квадратичные члены (и константу?) *)
  (* Гармонический гамильтониан = Кинетика + Квадратичный потенциал *)
  
  hHarmonic = kinetic + potentialExp;
  
  hHarmonic
];


BuildInductanceMatrix[hamiltonian_, topology_] := 
 Module[{nodes, fluxVars, potential, hessian, n},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  n = Length[nodes];
  
  potential = hamiltonian /. _Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* L^-1_ij = ∂²U / ∂φ_i ∂φ_j *)
  hessian = Table[
    D[potential, fluxVars[[i]], fluxVars[[j]]],
    {i, n}, {j, n}
  ];
  
  hessian
];

BuildPotentialGradient[hamiltonian_, topology_] := 
 Module[{nodes, fluxVars, potential},
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  potential = hamiltonian /. _Subscript[QED`$ChargeSymbol, _] -> 0;
  
  D[potential, {fluxVars}]
];


(* ════════════════════════════════════════════════════════════════ *)
(* ВОЛНОВЫЕ ФУНКЦИИ                                                 *)
(* ════════════════════════════════════════════════════════════════ *)

BuildHarmonicWavefunction[topology_Association, frequencies_List, 
                          effectiveCapacitances_List, projectionMatrix_?MatrixQ, 
                          quantumNumbers_List] := 
 Module[{nodes, fluxVars, minFluxVars, deltaPhi, normalCoords, 
         wavefunctions1D, normalization, psiTotal},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minFluxVars = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Shift to equilibrium *)
  deltaPhi = fluxVars - minFluxVars;
  
  (* Transform to normal coordinates q *)
  (* projectionMatrix K: q_normal = K . deltaPhi *)
  normalCoords = projectionMatrix . deltaPhi;
  
  (* Normalization factor: Jacobian of the transformation *)
  (* Integral |psi|^2 dPhi = 1 *)
  (* dPhi = (1/det K) dq *)
  (* If psi(q) is normalized, then psi(Phi) = Sqrt[det K] * psi(K.Phi) *)
  normalization = Sqrt[Abs[Det[projectionMatrix]]];
  
  (* 2. Build 1D wavefunctions for each mode *)
  wavefunctions1D = MapThread[
    Function[{q, n, omega, cap},
      Module[{invLength2, hermite, gaussian, norm1D},
        
        (* Parameter alpha^2 = m*omega/hbar. Here mass m = cap *)
        invLength2 = (cap * omega) / hbar;
        
        (* Hermite polynomial H_n(sqrt(alpha)*q) *)
        hermite = HermiteH[n, Sqrt[invLength2] * q];
        
        (* Gaussian exp(-alpha*q^2/2) *)
        gaussian = Exp[-invLength2 * q^2 / 2];
        
        (* Normalization for 1D oscillator: 1/sqrt(2^n * n! * sqrt(pi/alpha)) *)
        (* Note: (pi/alpha)^(1/4) comes from integral. *)
        norm1D = 1 / Sqrt[2^n * Factorial[n]] * (invLength2 / Pi)^(1/4);
        
        norm1D * hermite * gaussian
      ]
    ],
    {normalCoords, quantumNumbers, frequencies, effectiveCapacitances}
  ];
  
  (* Total wavefunction *)
  psiTotal = normalization * Times @@ wavefunctions1D;
  
  psiTotal
];

End[];
EndPackage[];