BeginPackage["QED`Scattering`", {"QED`Model`"}];

BuildSymbolicScattering::usage = "BuildSymbolicScattering[topology, primaryParams, options] computes symbolic S-matrix. Note: Ignores topological GroundNode (floating ground assumption).";

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

(* ════════════════════════════════════════════════════════════════ *)
(* ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ *)
(* ════════════════════════════════════════════════════════════════ *)

GetEffectiveInductances[model_Association, explicitFluxes : (_List | Automatic) : Automatic] := 
 Module[{topology, primary, subRules, components, groundNode, 
        phiMin, phi0Val},
  
  topology = model["Topology"];
  primary = model["Primary"]; 
  components = topology["Components"];
  subRules = model["SubstitutionRules"];
  groundNode = topology["GroundNode"]; (* Для потоков земля все еще важна! *)
  phi0Val = QED`$Phi0Value;
  
  phiMin = If[explicitFluxes === Automatic,
     QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"],
     explicitFluxes
  ];
  
  If[phiMin === $Failed, Return[$Failed]];
  
  Cases[components, 
    {type_, n1_, n2_, name_, ___} /; type === "JosephsonJunction" :> 
     Module[{params, ejVal, phi1, phi2, phiDiff, phaseDrop, cosPhi, lVal},
         params = primary[name];
         ejVal = (params["EJ"]["Symbol"] /. subRules); 
         
         (* Тут мы используем КВАНТОВУЮ землю для расчета фаз *)
         phi1 = If[n1 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n1] /. phiMin];
         phi2 = If[n2 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n2] /. phiMin];
         
         phiDiff = (phi1 - phi2);
         phaseDrop = 2 * Pi * phiDiff / phi0Val;
         cosPhi = Cos[phaseDrop];
         
         If[cosPhi <= 10^-6, cosPhi = 10^-6];
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