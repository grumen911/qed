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
"BuildHarmonicWavefunction[topology, frequencies, effectiveCapacitances, transformationMatrix, quantumNumbers] \
constructs the analytical harmonic oscillator wavefunction in the original flux coordinates.

Arguments:
  topology: Association describing the circuit topology
  frequencies: List of normal mode frequencies {ω₁, ω₂, ...}
  effectiveCapacitances: List of effective capacitances {C₁, C₂, ...} for each mode
  transformationMatrix: Matrix T such that δφ_lab = T . q_normal, where δφ are flux deviations from equilibrium.
  quantumNumbers: List of integers {n₁, n₂, ...} specifying the state

Returns:
  Symbolic expression Ψ(φ₁, φ₂, ...). Includes Jacobian normalization factor.";

BuildCurrentOperator::usage = "BuildCurrentOperator[hamiltonian, topology] computes the harmonic approximation (linearized) \
of the circulating current operator I = -dH/dPhi_ext around the equilibrium flux positions.";

BuildVoltageOperator::usage = "BuildVoltageOperator[hamiltonian, topology, nodeIndex] computes the symbolic voltage operator \
V = dH/dq for a specific node, capturing the full capacitive coupling structure.";

BuildDynamicInductanceRules::usage = "BuildDynamicInductanceRules[topology, primaryParams, potential] \
builds replacement rules that express effective Josephson inductances L_EJ as functions of equilibrium phases.";

BuildZeroModeTransform::usage = "BuildZeroModeTransform[topology] generates coordinate transformation rules to eliminate zero modes.";

BuildCharacteristicEquation::usage = "BuildCharacteristicEquation[capMatrix, invIndMatrix] builds the characteristic equation \
det(L^-1 - \[Omega]^2 C) = 0 for the closed system's eigenfrequencies and extracts its coefficients.";

Begin["`Private`"];


(* Используем глобальные константы из QED` *)
phi0 = QED`$Phi0;  
hbar = QED`$hbar;


(* Все узлы кроме земли (используются для полной матрицы емкостей) *)
getAllIndependentNodes[topology_Association] := 
  Cases[topology["Nodes"], Except[topology["GroundNode"]]];

(* Только активные узлы, без якорей нулевых мод (используются для Гамильтониана) *)
getActiveNodes[topology_Association] := Module[{nodes, zeroModes},
  nodes = getAllIndependentNodes[topology];
  (* Предполагается, что CircuitTopology сохраняет список узлов-якорей *)
  zeroModes = Lookup[topology["GraphStructure"], "ZeroModeNodes", {}];
  Complement[nodes, zeroModes]
];


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


BuildCapacitanceMatrix[lagrangianTransformed_, topology_Association] := 
 Module[{allNodes, activeNodes, allPhiDotVars, capMatrixFull, 
         nonZeroIndices, capMatrixSolid, invCapMatrixSolid, invCapMatrixFull,
         activeIndices, reducedInvCapMatrix, effCapMatrix},
         
  allNodes = getAllIndependentNodes[topology];
  activeNodes = getActiveNodes[topology];
  
  allPhiDotVars = Derivative[1][Subscript[QED`$FluxSymbol, #]][t] & /@ allNodes;
  
  (* 1. Полная матрица емкостей (N x N) в новых координатах *)
  capMatrixFull = Outer[
    D[D[lagrangianTransformed, #1], #2] &,
    allPhiDotVars,
    allPhiDotVars
  ];
  
  (* 2. Находим узлы, у которых есть хоть какая-то емкость (диагональ != 0) *)
  nonZeroIndices = Select[Range[Length[allNodes]], capMatrixFull[[#, #]] =!= 0 &];
  
  If[Length[nonZeroIndices] == 0,
    Return[ConstantArray[0, {Length[activeNodes], Length[activeNodes]}]];
  ];
  
  (* 3. Вырезаем невырожденную часть и обращаем её *)
  capMatrixSolid = capMatrixFull[[nonZeroIndices, nonZeroIndices]];
  invCapMatrixSolid = Inverse[capMatrixSolid];
  
  (* 4. Возвращаем нули на место для вырожденных (пустых) переменных *)
  invCapMatrixFull = ConstantArray[0, Dimensions[capMatrixFull]];
  invCapMatrixFull[[nonZeroIndices, nonZeroIndices]] = invCapMatrixSolid;
  
  (* 5. Выделяем блок матрицы обратных емкостей, соответствующий только активным узлам *)
  activeIndices = Flatten[Map[FirstPosition[allNodes, #]&, activeNodes]];
  reducedInvCapMatrix = invCapMatrixFull[[activeIndices, activeIndices]];
  
  (* 6. Итоговая матрица емкостей (N_act x N_act) - это обратная к редуцированной *)
  effCapMatrix = Simplify[Inverse[reducedInvCapMatrix]];
  
  effCapMatrix
 ];


(*
  Physics: Inverse inductance matrix L⁻¹ (stiffness matrix) from direct Hessian.
  
  For harmonic approximation around equilibrium:
  H ≈ H(φ_min) + (1/2) ∑ᵢⱼ L⁻¹ᵢⱼ (φᵢ - φᵢ,min)(φⱼ - φⱼ,min)
  
  where L⁻¹ᵢⱼ = ∂²H/∂φᵢ∂φⱼ|_{φ=φ_min} is the Hessian evaluated at equilibrium.
  
  CRITICAL: Must use direct Hessian from original Hamiltonian!
  
  Using Series-expanded harmonicHamiltonian produces incorrect matrix elements
  due to numerical errors in mixed derivatives (~10⁻⁸), which corrupt the 
  eigenspectrum and produce NON-PHYSICAL IMAGINARY FREQUENCIES.
  
  At a true minimum, Hessian must be positive-definite → all eigenvalues > 0.
  Imaginary frequencies (λ < 0) indicate either:
  1. Numerical artifact (if error ~ 10⁻⁸)
  2. Saddle point instead of minimum (requires investigation)
  
  This implementation computes Hessian symbolically, then substitutes φ_min,
  preserving positive-definiteness and avoiding imaginary frequencies.
  
  Reference: Devoret lectures, Les Houches (2004), Section 3.2
             Koch et al., PRA 76, 042319 (2007)
*)

BuildInductanceMatrix[hamiltonian_, topology_Association] := 
 Module[{nodes, phiVars, minSymbols, hessianSymbolic},
  
  nodes = getActiveNodes[topology];
  phiVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Step 1: Compute symbolic Hessian ∂²H/∂φᵢ∂φⱼ from original Hamiltonian *)
  (* This avoids numerical errors from Series expansion *)
  hessianSymbolic = D[hamiltonian, {phiVars, 2}];
  
  (* Step 2: Substitute φ → φ_min symbolically *)
  hessianSymbolic = hessianSymbolic /. Thread[phiVars -> minSymbols];
  
  (* Step 3: Simplify coefficients *)
  Simplify[hessianSymbolic]
 ];

BuildHamiltonian[lagrangianTransformed_, capMatrixActive_, topology_Association] := 
 Module[{activeNodes, activeQVars, invCapMatrixActive, 
         kineticEnergy, potentialEnergy},
         
  activeNodes = getActiveNodes[topology];
  activeQVars = Subscript[QED`$ChargeSymbol, #] & /@ activeNodes;
  
  (* 1. Обращаем матрицу емкостей (она уже правильного размера N_act x N_act) *)
  invCapMatrixActive = Inverse[capMatrixActive];
  
  (* 2. Строим кинетическую энергию только для активных переменных *)
  kineticEnergy = (1/2) * activeQVars . invCapMatrixActive . activeQVars;
  
  (* 3. Потенциальная энергия: УНИВЕРСАЛЬНО зануляем ЛЮБЫЕ скорости (производные) *)
  potentialEnergy = -lagrangianTransformed /. Derivative[1][_][_] -> 0;
  
  Collect[kineticEnergy + potentialEnergy, Join[Subscript[QED`$FluxSymbol, #] & /@ activeNodes, activeQVars], Simplify]
 ];

(*
  Physics: Gradient of potential energy for equilibrium conditions.
  
  Equilibrium fluxes satisfy ∇U = 0, where U(φ) is the potential energy.
  For Josephson circuits:
  
  ∂U/∂φᵢ = ∑ⱼ (EJ/Φ₀) sin(2π(φᵢ - φⱼ)/Φ₀ + δᵢⱼ)
  
  where δᵢⱼ accounts for external flux in loops.
  
  This gradient is used in FindRoot-based numerical minimization to find
  all equilibrium points (minima, maxima, saddles) by solving ∇U = 0.
  
  Reference: Devoret lectures (2004), Section 2.3
*)

BuildPotentialGradient[hamiltonian_, topology_Association] := 
 Module[{nodes, fluxVars, potential, gradient},
  
  nodes = getActiveNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Потенциальная энергия: U(φ) = H(q=0, φ) *)
  potential = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Градиент: ∇U = {∂U/∂φ₁, ∂U/∂φ₂, ∂U/∂φ₃} *)
  gradient = D[potential, #] & /@ fluxVars;
  
  Simplify[gradient]
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

(* В src/Analytic/Analytic.wl *)

BuildHarmonicHamiltonian[hamiltonian_, topology_Association] := 
 Module[{nodes, fluxVars, minSymbols, deltas, t, seriesTotalDeg},
  
  nodes = getActiveNodes[topology];
  
  (* Исходные переменные потока *)
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  
  (* Символы минимума *)
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Отклонения от минимума: d_i = phi_i - phi_min_i *)
  deltas = fluxVars - minSymbols;
  
  (* Метод t-scaling (Total Degree Truncation):
     1. Параметризуем смещение: phi = phi_min + t * delta
     2. Раскладываем по t до 2-го порядка.
     3. Полагаем t -> 1.
     Это гарантирует, что остаются только члены с суммарной степенью 
     отклонений <= 2.
  *)
  
  seriesTotalDeg = Normal @ Series[
    hamiltonian /. Thread[fluxVars -> (minSymbols + t * deltas)],
    {t, 0, 2}
  ] /. t -> 1;

  (* Группируем результат для читаемости *)
  Collect[seriesTotalDeg, Join[fluxVars, Subscript[QED`$ChargeSymbol, #] & /@ nodes], Simplify]
 ];


(* ════════════════════════════════════════════════════════════════ *)
(*                 HARMONIC WAVEFUNCTIONS                           *)
(* ════════════════════════════════════════════════════════════════ *)

BuildHarmonicWavefunction[
    topology_Association, 
    frequencies_List, 
    effectiveCapacitances_List, 
    transformationMatrix_?MatrixQ, 
    quantumNumbers_List
] := Module[{
    nodes, fluxVars, minFluxVars,
    deltaPhi,     (* Vector of flux deviations *)
    normalCoords, (* Vector of normal coordinates q *)
    invT,         (* Inverse transformation matrix *)
    detT,         (* Jacobian determinant *)
    wavefunctions1D,
    psiTotal,
    nDOF
},
    (* 0. Generate variables from topology *)
    nodes = getActiveNodes[topology];
    fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
    minFluxVars = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
    nDOF = Length[nodes];
    
    (* Check consistency *)
    If[Length[frequencies] != nDOF, Message[BuildHarmonicWavefunction::dim, "frequencies", Length[frequencies], nDOF]; Return[$Failed]];
    If[Length[effectiveCapacitances] != nDOF, Message[BuildHarmonicWavefunction::dim, "effectiveCapacitances", Length[effectiveCapacitances], nDOF]; Return[$Failed]];
    If[Length[transformationMatrix] != nDOF, Message[BuildHarmonicWavefunction::dim, "transformationMatrix", Length[transformationMatrix], nDOF]; Return[$Failed]];
    
    
    (* 1. Coordinate Transformation *)
    (* The transformationMatrix T (FluxTransform) converts from normal mode coordinates to lab coordinates: *)
    (* δφ_lab = T . q_norm *)

    deltaPhi = fluxVars - minFluxVars;
    
    (* To express normal coordinates q in terms of lab coordinates φ, use inverse of T *)
    normalCoords = Inverse[transformationMatrix] . deltaPhi;
    
    (* Jacobian of transformation φ -> q *)
    (* The transformation from normal (q) to lab (φ) coordinates is δφ = T . q *)
    (* The volume elements are related by dφ = |det(T)| dq *)
    (* Normalization: ∫|ψ(φ)|² dφ = 1 => ∫|ψ_norm(q)|² |det(T)| dq = 1 *)
    (* So, if ψ_norm(q) is the normalized wavefunction in normal coords, *)
    (* the wavefunction in lab coords is ψ(φ) = ψ_norm(q(φ)) / Sqrt[Abs[Det[T]]] *)
    
    detT = Abs[Det[transformationMatrix]];
    
    (* 2. Build 1D wavefunctions for each mode *)
    wavefunctions1D = MapThread[
        Function[{q, n, omega, cap},
            Module[{invLength2, normFactor, gaussian, poly, xi},
                (* m = cap, freq = omega *)
                (* Width parameter α² = mω/ℏ *)
                
                invLength2 = (cap * omega) / hbar;
                xi = Sqrt[invLength2] * q;
                
                (* Normalization factor for 1D oscillator *)
                (* (α²/π)^(1/4) / sqrt(2^n n!) *)
                normFactor = (invLength2 / Pi)^(1/4) / Sqrt[2^n * Factorial[n]];
                
                gaussian = Exp[-invLength2 * q^2 / 2];
                poly = HermiteH[n, xi];
                
                normFactor * gaussian * poly
            ]
        ],
        {normalCoords, quantumNumbers, frequencies, effectiveCapacitances}
    ];
    
    (* 3. Combine with Jacobian factor *)
    psiTotal = (1 / Sqrt[detT]) * Times @@ wavefunctions1D;
    
    psiTotal
];

BuildHarmonicWavefunction::dim = "Dimension mismatch: `1` has length `2`, expected `3`.";
 
BuildCurrentOperator[hamiltonian_, topology_Association] := 
 Module[{exactCurrent, nodes, fluxVars, minSymbols, deltas, t, currentSeries},
  
  (* 1. Точный оператор: I = -dH/dPhi_ext *)
  exactCurrent = Simplify[-D[hamiltonian, QED`$PhiExt]];
  
  (* 2. Подготовка переменных для разложения *)
  nodes = getActiveNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Отклонения от равновесия: d_i = phi_i - phi_min_i *)
  deltas = fluxVars - minSymbols;
  
  (* 3. Разложение в ряд (Linearized Current)
     Используем t-scaling (phi -> phi_min + t*delta) и берем 1-й порядок по t.
     Нулевой порядок (Const) - это статический ток в рабочей точке.
     Первый порядок (Linear) - это оператор шума, вызывающий переходы.
  *)
  currentSeries = Normal @ Series[
    exactCurrent /. Thread[fluxVars -> (minSymbols + t * deltas)],
    {t, 0, 1} (* Ограничиваемся линейным членом, т.к. H - квадратичный *)
  ] /. t -> 1;
  
  Simplify[currentSeries]
 ];

BuildVoltageOperator[hamiltonian_, topology_, nodeIndex_Integer] := Module[
  {chargeSym},
  
  (* Символ заряда для данного узла *)
  chargeSym = Subscript[QED`$ChargeSymbol, nodeIndex];
  
  (* Согласно уравнениям Гамильтона: V_node = dH / dq_node.
     Для стандартных цепей это вернет линейную комбинацию зарядов: Sum[(C^-1)_nj * q_j].
     Для нелинейных емкостей это вернет корректный нелинейный оператор.
  *)
  Simplify[D[hamiltonian, chargeSym]]
];

BuildDynamicInductanceRules[topology_, primaryParams_, potential_] := 
 Module[{rules},
  Flatten @ DeleteMissing @ Map[
    Function[comp,
      If[comp[[1]] === "JosephsonJunction",
        Module[{name, ejSym, cosArgs, lEjSym, minRule},
          name = comp[[4]]; 
          ejSym = primaryParams[name]["EJ"]["Symbol"]; 
          
          (* Ищем аргумент косинуса в потенциале, который умножается на этот E_J *)
          cosArgs = Cases[potential, a_. * ejSym * Cos[arg_] :> arg, Infinity];
          
          If[Length[cosArgs] > 0,
            (* Формируем символ эффективной индуктивности *)
            lEjSym = Subscript[QED`$InductanceSymbol, name];
            
            (* Правило перевода обычных потоков узлов в равновесные (с индексом "min") *)
            minRule = (Subscript[QED`$FluxSymbol, i_] :> Subscript[QED`$FluxSymbol, "min", i]);
            
            (* Возвращаем правило замены *)
            lEjSym -> (QED`$Phi0^2 / (4 * Pi^2 * ejSym * Cos[cosArgs[[1]] /. minRule]))
          ,
            Missing[]
          ]
        ],
        Missing[]
      ]
    ],
    topology["Components"]
  ]
 ];

BuildZeroModeTransform[topology_Association] := Module[
  {islands, fluxRules, fluxDotRules, zeroModeNodes},
  
  islands = Lookup[topology["GraphStructure"], "FloatingIslands", {}];
  zeroModeNodes = {};
  fluxRules = {};
  fluxDotRules = {};
  
  Do[
    Module[{anchor = island[[1]], others = Rest[island], anchorSym, anchorDotSym},
      anchorSym = Subscript[QED`$FluxSymbol, anchor];
      anchorDotSym = Derivative[1][Subscript[QED`$FluxSymbol, anchor]][t];
      
      AppendTo[zeroModeNodes, anchor];
      
      (* Все остальные узлы острова выражаются через якорь (центр масс) *)
      Do[
        AppendTo[fluxRules, Subscript[QED`$FluxSymbol, node] -> Subscript[QED`$FluxSymbol, node] + anchorSym];
        AppendTo[fluxDotRules, Derivative[1][Subscript[QED`$FluxSymbol, node]][t] -> Derivative[1][Subscript[QED`$FluxSymbol, node]][t] + anchorDotSym];
      , {node, others}];
    ]
  , {island, islands}];
  
  <|
    "ZeroModeNodes" -> zeroModeNodes,
    "FluxRules" -> fluxRules,
    "FluxDotRules" -> fluxDotRules
  |>
];

BuildCharacteristicEquation[capMatrix_, invIndMatrix_] := Module[
  {omegaSym, matrix, detPoly, eqCoeffs, alphaRules, charEq},
  
  omegaSym = Symbol["\[Omega]"];
  
  (* 1. Вычисляем детерминант: det(L^-1 - \[Omega]^2 * C) *)
  matrix = invIndMatrix - omegaSym^2 * capMatrix;
  detPoly = Simplify[Det[matrix]];
  
  (* 2. Извлекаем коэффициенты при степенях \[Omega] *)
  eqCoeffs = CoefficientList[detPoly, omegaSym];
  
  (* 3. Распределяем коэффициенты по греческим буквам *)
  alphaRules = <||>;
  If[Length[eqCoeffs] > 0, alphaRules["\[Gamma]"] = Simplify[eqCoeffs[[1]]]];
  If[Length[eqCoeffs] > 2, alphaRules["\[Alpha]"] = Simplify[-eqCoeffs[[3]]]]; (* Знак минус для формата -\[Alpha]\[Omega]^2 *)
  If[Length[eqCoeffs] > 4, alphaRules["\[Beta]"]  = Simplify[eqCoeffs[[5]]]];
  If[Length[eqCoeffs] > 6, alphaRules["\[Eta]"]   = Simplify[eqCoeffs[[7]]]];
  
  (* Автоматическая индексация для степеней выше 6 (\[Omega]^8 и т.д.) *)
  Do[
    If[eqCoeffs[[i]] =!= 0, alphaRules["\[Alpha]" <> ToString[i-1]] = Simplify[eqCoeffs[[i]]]],
    {i, 9, Length[eqCoeffs], 2}
  ];
  
  (* 4. Собираем красивое символьное уравнение *)
  charEq = If[Length[eqCoeffs] > 0,
    Sum[
      Switch[i-1,
        0, Symbol["\[Gamma]"],
        2, -Symbol["\[Alpha]"] * omegaSym^2,
        4, Symbol["\[Beta]"] * omegaSym^4,
        6, Symbol["\[Eta]"] * omegaSym^6,
        _, Subscript[Symbol["\[Alpha]"], i-1] * omegaSym^(i-1)
      ],
      {i, 1, Length[eqCoeffs], 2}
    ] == 0,
    True
  ];
  
  <|
    "CharacteristicEquation" -> charEq,
    "CharacteristicCoefficients" -> alphaRules
  |>
];

End[];
EndPackage[];