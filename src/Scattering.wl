BeginPackage["QED`Scattering`", {"QED`Model`"}];

BuildSymbolicScattering::usage = "BuildSymbolicScattering[topology, primaryParams, options] computes symbolic S-matrix. \
Note: Ignores topological GroundNode (floating ground assumption).";

BuildSymbolicBICCondition::usage = "BuildSymbolicBICCondition[sMatrixAssoc] computes the analytical conditions \
for Bound States in the Continuum (BIC). It extracts the determinant of the S-matrix, treats it as a polynomial \
in Z0, and eliminates the frequency variable to find Z0-independent purely imaginary roots.";

ComputeNumericalScattering::usage = "ComputeNumericalScattering[model, frequencyList] computes numerical S-parameters.";

GetEffectiveInductances::usage = "GetEffectiveInductances[model, explicitFluxes] returns effective inductance rules.";

Begin["`Private`"];

(* ════════════════════════════════════════════════════════════════ *)
(* СИМВОЛЬНОЕ ПОСТРОЕНИЕ *)
(* ════════════════════════════════════════════════════════════════ *)

Options[BuildSymbolicScattering] = {
  Ports -> {1,2}, (* Automatic *)
  ReferenceImpedance -> 50,
  IgnoreJunctionCapacitance -> False
};

Options[BuildSymbolicBICCondition] = {
  SimplificationRules -> {}
};

BuildSymbolicScattering[topology_Association, primaryParams_Association, opts : OptionsPattern[]] := 
 Module[{
    nodes, activeNodes, nNodes,
    components, yMatrix, portIndices, z0, ignoreCap,
    sVar, internalIndices, 
    Ypp, Ypi, Yip, Yii, Yschur, 
    unitMatrix, rawSMatrix, sMatrix,
    nodeCounts, candidatePorts, allCompNodes
 },
  
  components = topology["Components"];
  
  (* ИЗМЕНЕНИЕ 1: Игнорируем GroundNode. Берем ВСЕ узлы, участвующие в компонентах. *)
  (* Для S-матрицы земля - это внешний "висящий" потенциал. *)
  nodes = Union[Flatten[components[[All, 2 ;; 3]]]];
  
  (* Все узлы схемы являются активными (potential != 0 относительно внешней земли) *)
  activeNodes = nodes; 
  nNodes = Length[activeNodes];
  
  z0 = OptionValue[ReferenceImpedance];
  ignoreCap = TrueQ[OptionValue[IgnoreJunctionCapacitance]];
  sVar = Symbol["s"]; 
  
  (* ИЗМЕНЕНИЕ 2: Автопоиск портов теперь ищет по ВСЕМ узлам *)
  If[OptionValue[Ports] === Automatic,
     (* Считаем вхождения *)
     allCompNodes = Flatten[components[[All, 2 ;; 3]]];
     nodeCounts = Counts[allCompNodes];
     
     (* Порт = Узел, к которому подключен только 1 компонент *)
     candidatePorts = Select[activeNodes, (Lookup[nodeCounts, #, 0] == 1) &];
     
     If[Length[candidatePorts] >= 1,
        portIndices = Flatten[FirstPosition[activeNodes, #] & /@ candidatePorts],
        portIndices = {1, nNodes} (* Fallback: берем первый и последний из списка *)
     ];
  ,
     (* Если порты заданы вручную, ищем их индексы в полном списке nodes *)
     portIndices = Flatten[FirstPosition[activeNodes, #] & /@ OptionValue[Ports]]
  ];

  (* Размер матрицы теперь равен полному количеству узлов *)
  yMatrix = ConstantArray[0, {nNodes, nNodes}];
  
  (* ИЗМЕНЕНИЕ 3: Заполнение матрицы без исключения GroundNode *)
  Do[
    Module[{type, n1, n2, name, u, v, val, sym, symC, valC, valL},
      {type, n1, n2, name} = comp[[1;;4]];
      
      (* Ищем индексы узлов в списке activeNodes *)
      u = FirstPosition[activeNodes, n1][[1]];
      v = FirstPosition[activeNodes, n2][[1]];
      
      (* Вычисляем проводимость ветви *)
      val = Switch[type,
        "Capacitor",
           sym = primaryParams[name]["C"]["Symbol"];
           sVar * sym,
           
        "Inductor",
           sym = primaryParams[name]["L"]["Symbol"];
           1 / (sVar * sym),
           
        "JosephsonJunction",
           symC = primaryParams[name]["CJ"]["Symbol"];
           valC = If[ignoreCap, 0, sVar * symC];
           valL = 1 / (sVar * Subscript["L", name]);
           valC + valL,
           
        _, 0
      ];

      (* Заполняем матрицу. Узлов "земли" внутри схемы нет, поэтому u и v всегда > 0 *)
      yMatrix[[u, u]] += val;
      yMatrix[[v, v]] += val;
      yMatrix[[u, v]] -= val;
      yMatrix[[v, u]] -= val;
    ],
    {comp, components}
  ];

  (* 3. Свертка (Schur Complement) - сворачиваем все узлы, кроме портов *)
  internalIndices = Complement[Range[nNodes], portIndices];
  
  If[Length[internalIndices] > 0,
    Ypp = yMatrix[[portIndices, portIndices]];
    Yii = yMatrix[[internalIndices, internalIndices]];
    Ypi = yMatrix[[portIndices, internalIndices]];
    Yip = yMatrix[[internalIndices, portIndices]];
    
    Yschur = Simplify[Ypp - Ypi . Inverse[Yii] . Yip];
  ,
    Yschur = yMatrix[[portIndices, portIndices]];
  ];

  unitMatrix = IdentityMatrix[Length[portIndices]];
  
  (* 4. S-Matrix *)
  rawSMatrix = (unitMatrix - z0 * Yschur) . Inverse[unitMatrix + z0 * Yschur];
  
  (* Упрощение *)
  sMatrix = Map[
    Function[expr,
      Module[{frac = Together[expr]}, 
         Collect[Numerator[frac], sVar] / Collect[Denominator[frac], sVar]
      ]
    ], 
    rawSMatrix, 
    {2}
  ];

  <|
    "YMatrixFull" -> yMatrix,
    "YMatrixReduced" -> Yschur,
    "SMatrix" -> sMatrix,       
    "SMatrixRaw" -> rawSMatrix, 
    "FrequencyVariable" -> sVar,
    "PortIndices" -> portIndices,
    "ActiveNodes" -> activeNodes (* Теперь это полный список узлов *)
  |>
 ];

BuildSymbolicBICCondition[sMatrixAssoc_Association, opts : OptionsPattern[]] := 
 Module[{sMat, sVar, z0Var, xVar, detS, charPoly, coeffs, processPoly, 
         rawSystem, rules, simplifiedSystem, cond},
  
  sMat = sMatrixAssoc["SMatrix"];
  sVar = sMatrixAssoc["FrequencyVariable"];
  z0Var = QED`$Z0;
  xVar = Symbol["x"]; (* Переменная для x = s^2 = -\omega^2 *)
  
  (* 1. Вычисляем детерминант S-матрицы *)
  detS = Det[sMat];
  
  (* 2. Берем только числитель (избавляемся от знаменателей) *)
  charPoly = Numerator[Together[detS]];
  
  (* 3. Извлекаем коэффициенты при степенях Z0 *)
  coeffs = CoefficientList[Expand[charPoly], z0Var];
  
  (* 4. Вспомогательная функция: разделяет четность и делает замену s^2 -> x *)
  processPoly[p_] := Module[{even, odd, pX},
    even = Simplify[(p + (p /. sVar -> -sVar))/2];
    odd = Simplify[(p - (p /. sVar -> -sVar))/(2 * sVar)];
    pX = Simplify[even + odd];
    pX = pX /. {sVar^n_Integer /; EvenQ[n] :> xVar^(n/2), sVar^2 -> xVar};
    Simplify[pX]
  ];
  
  (* 5. ПОЛНАЯ СИСТЕМА (Стратегия Б) - для численных сканирований *)
  rawSystem = DeleteCases[Simplify[processPoly /@ coeffs], 0];
  
  (* 6. ПРИМЕНЕНИЕ ПРАВИЛ (Стратегия В) - для аналитики *)
  rules = OptionValue[SimplificationRules];
  simplifiedSystem = DeleteCases[Simplify[rawSystem /. rules], 0];
  
  (* 7. Исключаем x для получения финального аналитического условия *)
  cond = If[Length[simplifiedSystem] >= 2,
    Simplify[Eliminate[Thread[simplifiedSystem == 0], xVar]],
    If[Length[simplifiedSystem] == 1,
        simplifiedSystem[[1]] == 0,
        True
    ]
  ];
  
  <|
    "FullSystem" -> Thread[rawSystem == 0],         (* Строгая система из 2 уравнений *)
    "SimplifiedSystem" -> Thread[simplifiedSystem == 0], (* Система после зануления CJ *)
    "Variable" -> xVar,                             (* Искомая частота x = -omega^2 *)
    "Condition" -> cond,                            (* Итоговое аналитическое условие (Eliminate) *)
    "RulesApplied" -> rules
  |>
 ];

(* ════════════════════════════════════════════════════════════════ *)
(* ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ *)
(* ════════════════════════════════════════════════════════════════ *)

GetEffectiveInductances[model_Association, runtimeRules : (_List | Automatic) : Automatic] := 
 Module[{topology, primary, subRules, components, groundNode, 
        phi0Val, fluxLoops, effectiveRules},
  
  topology = model["Topology"];
  primary = model["Primary"]; 
  components = topology["Components"];
  subRules = model["SubstitutionRules"];
  groundNode = topology["GroundNode"];
  phi0Val = QED`$Phi0Value;
  
  fluxLoops = Lookup[topology["GraphStructure"], "fluxLoops", <||>];
  
  (* 1. ФОРМИРОВАНИЕ ПРАВИЛ *)
  effectiveRules = If[runtimeRules === Automatic,
     Module[{eqFluxes},
        eqFluxes = QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
        
        (* FIX: Если ошибка, возвращаем $Failed как значение Module, 
           а снаружи проверяем результат *)
        If[eqFluxes === $Failed, 
            $Failed, 
            Join[eqFluxes, subRules]
        ]
     ],
     (* Свип: Переданные правила имеют приоритет *)
     Join[runtimeRules, subRules]
  ];

  (* Если не удалось получить правила, выходим *)
  If[effectiveRules === $Failed, Return[$Failed]];
  
  Cases[components, 
    {type_, n1_, n2_, name_, ___} /;
    type === "JosephsonJunction" :> 
     Module[{params, ejVal, phi1, phi2, phiDiff, phaseDrop, cosPhi, lVal, 
             extFluxVal, loopInfo, fluxSym},
         
         params = primary[name];
         (* Используем ReplaceRepeated (//.) для разрешения цепочек *)
         ejVal = (params["EJ"]["Symbol"] //. effectiveRules); 
         
         phi1 = If[n1 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n1] //. effectiveRules];
         phi2 = If[n2 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n2] //. effectiveRules];

         extFluxVal = 0.;
         If[KeyExistsQ[fluxLoops, name],
            loopInfo = fluxLoops[name];
            fluxSym = loopInfo["ExternalFluxSymbol"];
            
            (* ГЛАВНОЕ: Ищем значение во всем контексте (//.) *)
            extFluxVal = (fluxSym //. effectiveRules);
            
            If[!NumericQ[extFluxVal], extFluxVal = 0.];
         ];
         
         phiDiff = (phi1 - phi2) + extFluxVal;
         phaseDrop = 2 * Pi * phiDiff / phi0Val;
         cosPhi = Cos[phaseDrop];
         
         If[Abs[cosPhi] <= 10^-6, cosPhi = Sign[cosPhi] * 10^-6];
         If[cosPhi == 0, cosPhi = 10^-6]; 

         lVal = (phi0Val / (2 * Pi))^2 / (ejVal * cosPhi);
         
         Subscript["L", name] -> lVal
     ]
  ]
 ];

ComputeNumericalScattering[model_Association, frequencyList_List] := 
 Module[{symScattering, components, primary, valRules, lJeffRules, 
        sMatrixExpr, sVarSym, sMatrixNumFunction, omegaList},
  
  symScattering = BuildSymbolicScattering[model["Topology"], model["Primary"]];
  
  components = model["Topology"]["Components"];
  primary = model["Primary"];
  
  valRules = Flatten @ Map[
    Function[comp,
      Module[{type, name, p},
        {type, name} = {comp[[1]], comp[[4]]};
        p = primary[name]; 
        Switch[type,
          "Capacitor", p["C"]["Symbol"] -> p["C"]["Value"],
          "Inductor",  p["L"]["Symbol"] -> p["L"]["Value"],
          "JosephsonJunction", p["CJ"]["Symbol"] -> p["CJ"]["Value"],
          _, {}
        ]
      ]
    ],
    components
  ];

  lJeffRules = GetEffectiveInductances[model];
  If[lJeffRules === $Failed, Return[$Failed]];
  
  sMatrixExpr = symScattering["SMatrixRaw"];
  sVarSym = symScattering["FrequencyVariable"];
  
  sMatrixNumFunction = sMatrixExpr /. Join[valRules, lJeffRules];
  
  omegaList = I * 2 * Pi * frequencyList;
  Map[
     Function[w, sMatrixNumFunction /. sVarSym -> w],
     omegaList
  ]
 ];

End[];
EndPackage[];