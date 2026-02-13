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

BuildAnharmonicPart::usage = "BuildAnharmonicPart[hamiltonian, topology, order] returns the symbolic \
Taylor expansion of the potential energy terms of order > 2.";

QuantizeToLadderOperators::usage = "QuantizeToLadderOperators[expr, topology, transformMatrix, frequencies, \
effectiveCapacitances] transforms flux variables into ladder operators Create[mode] and Annihilate[mode].";

CalculatePerturbationCorrection::usage = "CalculatePerturbationCorrection[operator, state] computes the \
first-order energy correction <state|operator|state>. State is a list of occupation numbers.";

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
  
  nodes = getIndependentNodes[topology];
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
  
  nodes = getIndependentNodes[topology];
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
  
  nodes = getIndependentNodes[topology];
  
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
    nodes = getIndependentNodes[topology];
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

(* --- Выделение ангармонической части --- *)
BuildAnharmonicPart[hamiltonian_, topology_Association, order_Integer:4] := 
 Module[{nodes, fluxVars, minSymbols, deltas, t, pot, series},
  
  nodes = getIndependentNodes[topology];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  deltas = fluxVars - minSymbols;
  
  (* Работаем только с потенциальной энергией (кинетическая квадратична) *)
  pot = hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0;
  
  (* Параметризуем отклонение от минимума *)
  series = Normal @ Series[
    pot /. Thread[fluxVars -> (minSymbols + t * deltas)],
    {t, 0, order}
  ];
  
  (* Оставляем только члены старше 2-го порядка (ангармонизм) *)
  series = Select[series, Exponent[#, t] > 2 &];
  
  Simplify[series /. t -> 1]
 ];


(* --- Переход к операторам (Квантование) --- *)
QuantizeToLadderOperators[expr_, topology_Association, T_?MatrixQ, freqs_List, caps_List] := 
 Module[{nodes, fluxVars, minFluxVars, deltaPhi, nDOF, qSyms, phiExpr, qRules, ladderExpr,
         CreateOp, AnnihilateOp},
  
  nodes = getIndependentNodes[topology];
  nDOF = Length[nodes];
  fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
  minFluxVars = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Временные символы для операторов, чтобы избежать конфликтов контекста *)
  CreateOp = Symbol["QED`Analytic`Create"];
  AnnihilateOp = Symbol["QED`Analytic`Annihilate"];

  (* 1. Выражаем лабораторные потоки через нормальные координаты q *)
  (* deltaPhi = T . q *)
  qSyms = Table[Unique["q"], {nDOF}]; 
  deltaPhi = T . qSyms;
  
  (* Подставляем в выражение вместо (phi - phi_min) *)
  (* Заметьте: expr зависит от phi, поэтому заменяем phi -> phi_min + T.q *)
  phiExpr = expr /. Thread[fluxVars -> (minFluxVars + deltaPhi)];
  
  (* 2. Правила замены нормальных координат на операторы: q = x_zpf * (a + a+) *)
  qRules = Table[
    Module[{xZpf, opSum},
      (* ZPF = Sqrt[hbar / (2 C w)] *)
      xZpf = Sqrt[QED`$hbar / (2 * caps[[i]] * freqs[[i]])];
      
      (* Используем NonCommutativeMultiply (**) для сохранения порядка *)
      (* q ~ (a + a+) *)
      opSum = xZpf * (AnnihilateOp[i] ** 1 + CreateOp[i] ** 1);
      qSyms[[i]] -> opSum
    ],
    {i, nDOF}
  ];
  
  ladderExpr = phiExpr /. qRules;
  
  (* Раскрываем скобки с учетом некоммутативности *)
  ExpandNonCommutative[ladderExpr /. (x_ ** 1) -> x]
 ];


(* Хелпер для раскрытия некоммутативных скобок *)
ExpandNonCommutative[expr_] := 
 Distribute[expr, Plus, NonCommutativeMultiply] //. {
   (a_ * b_) ** c_ :> a * (b ** c) /; NumericQ[a],
   a_ ** (b_ * c_) :> b * (a ** c) /; NumericQ[b],
   NonCommutativeMultiply[x_] :> x
 };


(* --- Вычисление поправки <n|V|n> --- *)
CalculatePerturbationCorrection[operator_, state_List] := 
 Module[{terms},
   (* Разбиваем полином на отдельные слагаемые *)
   terms = If[Head[operator] === Plus, List @@ operator, {operator}];
   Total[EvaluateTerm[#, state] & /@ terms]
 ];

EvaluateTerm[term_, state_List] := 
 Module[{coeff, ops, currentChain, nList, factor, opHead, modeIdx, n},
  
  (* 1. Разделяем коэффициент и цепочку операторов *)
  (* Ожидаем структуру: Coeff * (Op1 ** Op2 ...) или Coeff * Op1 *)
  {coeff, ops} = ParseTerm[term];
  
  (* Если операторов нет (константа), поправка равна самому члену *)
  If[ops === {}, Return[term]]; 

  (* 2. Применяем операторы к состоянию |n1, n2...> справа налево *)
  nList = state; (* Копия чисел заполнения *)
  factor = 1;
  
  (* ops - это список, например {Create[1], Annihilate[1], ...} *)
  Do[
    opHead = Head[op]; 
    modeIdx = First[op]; (* Индекс моды *)
    n = nList[[modeIdx]];
    
    If[opHead === Symbol["QED`Analytic`Create"],
      (* a+|n> = sqrt(n+1)|n+1> *)
      factor *= Sqrt[n + 1];
      nList[[modeIdx]]++;
    ,
    If[opHead === Symbol["QED`Analytic`Annihilate"],
      (* a|n> = sqrt(n)|n-1> *)
      If[n == 0, Return[0]]; (* Уничтожение вакуума *)
      factor *= Sqrt[n];
      nList[[modeIdx]]--;
    ]];
  , {op, Reverse[ops]}]; (* Важно: Reverse, т.к. в A**B оператор B действует первым *)
  
  (* 3. Проецируем на начальное состояние <state|final_state> *)
  If[nList === state, coeff * factor, 0]
 ];

(* Разбор члена на (Coeff, {Operators}) *)
ParseTerm[term_] := Module[{nc, scalar},
  Switch[Head[term],
    NonCommutativeMultiply, {1, List @@ term},
    Times, 
      (* Ищем некоммутативную часть *)
      nc = SelectFirst[List @@ term, Head[#] === NonCommutativeMultiply &];
      If[!MissingQ[nc], 
        {DeleteCases[term, nc], List @@ nc},
        (* Если нет **, проверяем одиночные операторы *)
        nc = Select[List @@ term, MemberQ[{Symbol["QED`Analytic`Create"], Symbol["QED`Analytic`Annihilate"]}, Head[#]] &];
        If[Length[nc] > 0, 
           (* Внимание: в Times порядок сбит, но для одиночного оператора неважно. 
              Для нескольких без ** порядок неопределен, считаем это ошибкой квантования, но обработаем как есть *)
           {DeleteCases[term, Alternatives @@ nc], nc},
           {term, {}} (* Число *)
        ]
      ],
    Symbol["QED`Analytic`Create"], {1, {term}},
    Symbol["QED`Analytic`Annihilate"], {1, {term}},
    _, {term, {}}
  ]
]; 

End[];
EndPackage[];