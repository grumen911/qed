BeginPackage["QED`Scattering`"];

(* Публичные функции *)
BuildSymbolicScattering::usage = 
  "BuildSymbolicScattering[topology, options] строит символьную S-матрицу и матрицу проводимости (Y).
   Опции:
     Ports -> {nodeIdx1, nodeIdx2} (по умолчанию: {первый, последний})
     ReferenceImpedance -> 50 (Ом)
     IgnoreJunctionCapacitance -> False
   Возвращает ассоциацию с символьными матрицами и списком эффективных индуктивностей.";

ComputeNumericalScattering::usage = 
  "ComputeNumericalScattering[model, frequencyList] вычисляет численные значения S-матрицы 
   для списка частот. Использует закэшированные равновесные потоки из модели.";

GetEffectiveInductances::usage = 
  "GetEffectiveInductances[model] возвращает список численных значений эффективных индуктивностей 
   джозефсоновских переходов в рабочей точке.";

Begin["`Private`"];

(* Импорт констант *)
phi0 = QED`$Phi0; 

(* ════════════════════════════════════════════════════════════════ *)
(* СИМВОЛЬНОЕ ПОСТРОЕНИЕ (ANALYTIC)                                 *)
(* ════════════════════════════════════════════════════════════════ *)

Options[BuildSymbolicScattering] = {
  Ports -> Automatic, 
  ReferenceImpedance -> 50,
  IgnoreJunctionCapacitance -> False
};

BuildSymbolicScattering[topology_Association, opts : OptionsPattern[]] := 
 Module[{
    nodes, groundNode, activeNodes, nNodes,
    components, yMatrix, portIndices, z0, ignoreCap,
    sVar, (* s = i*omega *)
    internalIndices, 
    Ypp, Ypi, Yip, Yii, Yschur, 
    unitMatrix, sMatrix,
    effectiveInductanceSymbols
 },
  
  (* 1. Подготовка данных *)
  nodes = topology["Nodes"];
  groundNode = topology["GroundNode"];
  components = topology["Components"];
  
  (* Узлы без земли *)
  activeNodes = Cases[nodes, Except[groundNode]];
  nNodes = Length[activeNodes];
  
  (* Опции *)
  z0 = OptionValue[ReferenceImpedance];
  ignoreCap = TrueQ[OptionValue[IgnoreJunctionCapacitance]];
  
  (* Индексы портов *)
  portIndices = If[OptionValue[Ports] === Automatic,
      {1, nNodes}, (* По умолчанию: вход в 1-й активный, выход из последнего *)
      (* Мапинг имен узлов в индексы матрицы *)
      Flatten[FirstPosition[activeNodes, #] & /@ OptionValue[Ports]]
  ];
  
  (* Символ частоты *)
  sVar = Symbol["s"]; (* s = I * omega *)
  
  (* 2. Построение Полной Матрицы Проводимости (Node Admittance Matrix) *)
  yMatrix = ConstantArray[0, {nNodes, nNodes}];
  effectiveInductanceSymbols = <||>;
  
  (* Используем Do для мутации матрицы (MNA Stamp method) *)
  Do[
    Module[{type, n1, n2, name, params, u, v, yComp, lJeff},
      {type, n1, n2, name} = comp[[1;;4]];
      params = If[Length[comp] >= 5, comp[[5]], <||>];
      
      (* Индексы узлов: 0 если Земля, иначе индекс в activeNodes *)
      u = If[n1 === groundNode, 0, FirstPosition[activeNodes, n1][[1]]];
      v = If[n2 === groundNode, 0, FirstPosition[activeNodes, n2][[1]]];
      
      (* Если компонент закорочен сам на себя или висит в воздухе (оба 0) - пропускаем *)
      If[u == 0 && v == 0, Continue[]];

      (* Формула проводимости компонента y(s) *)
      yComp = Switch[type,
        "Capacitor",
          sVar * params["C"]["Symbol"],
          
        "Inductor",
          1 / (sVar * params["L"]["Symbol"]),
          
        "JosephsonJunction",
          (* Символ L_Jeff для конкретного перехода *)
          lJeff = Subscript[Symbol["LJeff"], name]; 
          effectiveInductanceSymbols[name] = lJeff;
          
          If[ignoreCap,
             1 / (sVar * lJeff),
             1 / (sVar * lJeff) + sVar * params["CJ"]["Symbol"]
          ],
          
        _, 0
      ];
      
      (* Заполнение матрицы (штамп) *)
      (* Диагональные элементы (сумма проводимостей, подключенных к узлу) *)
      If[u > 0, yMatrix[[u, u]] += yComp];
      If[v > 0, yMatrix[[v, v]] += yComp];
      
      (* Внедиагональные (отрицательная проводимость между узлами) *)
      If[u > 0 && v > 0, 
        yMatrix[[u, v]] -= yComp;
        yMatrix[[v, u]] -= yComp;
      ];
      
    ],
    {comp, components}
  ];

  (* 3. Редукция матрицы (Schur Complement) для портов *)
  internalIndices = Complement[Range[nNodes], portIndices];
  
  If[Length[internalIndices] > 0,
    Ypp = yMatrix[[portIndices, portIndices]];
    Yii = yMatrix[[internalIndices, internalIndices]];
    Ypi = yMatrix[[portIndices, internalIndices]];
    Yip = yMatrix[[internalIndices, portIndices]];
    
    (* Y_ports = Ypp - Ypi . Inv(Yii) . Yip *)
    Yschur = Simplify[Ypp - Ypi . Inverse[Yii] . Yip];
  ,
    Yschur = yMatrix[[portIndices, portIndices]];
  ];

  (* 4. Вычисление S-матрицы *)
  unitMatrix = IdentityMatrix[Length[portIndices]];
  
  sMatrix = Simplify[
    (unitMatrix - z0 * Yschur) . Inverse[unitMatrix + z0 * Yschur]
  ];

  <|
    "YMatrixFull" -> yMatrix,
    "YMatrixReduced" -> Yschur,
    "SMatrix" -> sMatrix,
    "EffectiveInductances" -> effectiveInductanceSymbols,
    "FrequencyVariable" -> sVar,
    "PortIndices" -> portIndices,
    "ActiveNodes" -> activeNodes
  |>
 ];


(* ════════════════════════════════════════════════════════════════ *)
(* ЧИСЛЕННАЯ ПОДСТАНОВКА (NUMERICAL)                                *)
(* ════════════════════════════════════════════════════════════════ *)

GetEffectiveInductances[model_Association] := 
 Module[{topology, subRules, components, groundNode, 
        phiMin, phi0Val},
  
  topology = model["Topology"];
  components = topology["Components"];
  subRules = model["SubstitutionRules"];
  groundNode = topology["GroundNode"];
  phi0Val = QED`$Phi0Value; 
  
  (* Равновесные потоки из кэша *)
  phiMin = QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
  If[phiMin === $Failed, Return[$Failed]];
  
  (* Используем Cases вместо Do/AppendTo для скорости и чистоты кода *)
  Cases[components, 
    {"JosephsonJunction", n1_, n2_, name_, params_} :> 
     Module[{ejVal, phi1, phi2, phiDiff, phaseDrop, cosPhi, lVal},
         
         (* Численное EJ *)
         ejVal = (params["EJ"]["Symbol"] /. subRules); 
         
         (* Потоки на узлах *)
         phi1 = If[n1 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n1] /. phiMin];
         phi2 = If[n2 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n2] /. phiMin];
         
         (* L_eff calculation *)
         phiDiff = (phi1 - phi2); 
         phaseDrop = 2 * Pi * phiDiff / phi0Val;
         cosPhi = Cos[phaseDrop];
         
         (* L_eff = (Phi0/2pi)^2 / (Ej * cos(phi)) *)
         lVal = (phi0Val / (2 * Pi))^2 / (ejVal * cosPhi);
         
         Subscript[Symbol["LJeff"], name] -> lVal
     ]
  ]
 ];


ComputeNumericalScattering[model_Association, frequencyList_List] := 
 Module[{symScattering, subRules, lJeffRules, sMatrixSym, sVarSym, sMatrixNumFunction, 
        omegaList},
  
  (* 1. Символьная матрица (с учетом опций модели, если нужно, пока дефолт) *)
  (* TODO: Передавать опции портов из модели, если они там будут храниться *)
  symScattering = BuildSymbolicScattering[model["Topology"]];
  
  (* 2. Правила подстановки *)
  subRules = model["SubstitutionRules"];
  lJeffRules = GetEffectiveInductances[model]; 
  
  If[lJeffRules === $Failed, Return[$Failed]];
  
  (* 3. Полная подстановка *)
  sMatrixSym = symScattering["SMatrix"];
  sVarSym = symScattering["FrequencyVariable"];
  
  (* Функция от s *)
  sMatrixNumFunction = sMatrixSym /. Join[subRules, lJeffRules];
  
  (* 4. Вычисление по частотам *)
  omegaList = I * 2 * Pi * frequencyList;
  
  (* Map вместо Table - часто быстрее для численных списков *)
  Map[
     Function[w, sMatrixNumFunction /. sVarSym -> w],
     omegaList
  ]
 ];

End[];
EndPackage[];