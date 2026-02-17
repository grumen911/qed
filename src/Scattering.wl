BeginPackage["QED`Scattering`", {"QED`Model`"}];

(* Публичные функции *)
BuildSymbolicScattering::usage = 
  "BuildSymbolicScattering[topology, primaryParams, options] строит символьную S-матрицу.
   Возвращает:
     \"SMatrix\" - упрощенная рациональная функция.
     \"SMatrixRaw\" - необработанная дробь для численной стабильности.";

ComputeNumericalScattering::usage = 
  "ComputeNumericalScattering[model, frequencyList] вычисляет S-параметры (численно).";

GetEffectiveInductances::usage = 
  "GetEffectiveInductances[model, explicitFluxes] возвращает правила для L_eff.";

Begin["`Private`"];

(* ════════════════════════════════════════════════════════════════ *)
(* СИМВОЛЬНОЕ ПОСТРОЕНИЕ *)
(* ════════════════════════════════════════════════════════════════ *)

Options[BuildSymbolicScattering] = {
  Ports -> Automatic, 
  ReferenceImpedance -> 50,
  IgnoreJunctionCapacitance -> False
};

BuildSymbolicScattering[topology_Association, primaryParams_Association, opts : OptionsPattern[]] := 
 Module[{
    nodes, groundNode, activeNodes, nNodes,
    components, yMatrix, portIndices, z0, ignoreCap,
    sVar, internalIndices, 
    Ypp, Ypi, Yip, Yii, Yschur, 
    unitMatrix, rawSMatrix, sMatrix,
    nodeCounts, candidatePorts, allCompNodes
 },
  
  nodes = topology["Nodes"];
  groundNode = topology["GroundNode"];
  components = topology["Components"];
  activeNodes = Cases[nodes, Except[groundNode]];
  nNodes = Length[activeNodes];
  
  z0 = OptionValue[ReferenceImpedance];
  ignoreCap = TrueQ[OptionValue[IgnoreJunctionCapacitance]];
  sVar = Symbol["s"]; 
  
  (* 1. Определение портов ЛОКАЛЬНО (без изменения Topology) *)
  If[OptionValue[Ports] === Automatic,
     (* Считаем, сколько раз каждый узел встречается в компонентах *)
     allCompNodes = Flatten[components[[All, 2 ;; 3]]];
     nodeCounts = Counts[allCompNodes];
     
     (* Порты = активные узлы, к которым подключен всего 1 компонент *)
     candidatePorts = Select[activeNodes, (Lookup[nodeCounts, #, 0] == 1) &];
     
     (* Если портов не нашли (например, кольцо), берем 1-й и N-й узел *)
     If[Length[candidatePorts] >= 1,
        portIndices = Flatten[FirstPosition[activeNodes, #] & /@ candidatePorts],
        portIndices = {1, nNodes}
     ];
  ,
     (* Если порты заданы вручную *)
     portIndices = Flatten[FirstPosition[activeNodes, #] & /@ OptionValue[Ports]]
  ];

  yMatrix = ConstantArray[0, {nNodes, nNodes}];
  
  (* 2. Заполнение матрицы (берем символы из primaryParams) *)
  Do[
    Module[{type, n1, n2, name, u, v, val, sym, symC, valC, valL},
      {type, n1, n2, name} = comp[[1;;4]];
      
      u = If[n1 === groundNode, 0, FirstPosition[activeNodes, n1][[1]]];
      v = If[n2 === groundNode, 0, FirstPosition[activeNodes, n2][[1]]];
      
      If[u == 0 && v == 0, Continue[]];

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
           (* Индуктивность перехода всегда динамическая -> Subscript["L", name] *)
           valL = 1 / (sVar * Subscript["L", name]);
           valC + valL,
           
        _, 0
      ];

      If[u > 0, yMatrix[[u, u]] += val];
      If[v > 0, yMatrix[[v, v]] += val];
      If[u > 0 && v > 0, 
        yMatrix[[u, v]] -= val;
        yMatrix[[v, u]] -= val;
      ];
    ],
    {comp, components}
  ];

  (* 3. Свертка (Schur Complement) *)
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
  
  (* Упрощение для аналитики *)
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
    "ActiveNodes" -> activeNodes
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
  groundNode = topology["GroundNode"];
  phi0Val = QED`$Phi0Value;
  
  (* Если потоки переданы явно (из Model.wl), используем их *)
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
  
  (* Здесь тоже вызываем с primaryParams *)
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