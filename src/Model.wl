(*BeginPackage["QED`Model`", {"QED`CircuitTopology`","QED`Numeric`","QED`Analytic`"}];*)
BeginPackage["QED`Model`"];

Needs["QED`CircuitTopology`"];
Needs["QED`Numeric`"];
Needs["QED`Analytic`"];

CreateCircuitModel::usage = "CreateCircuitModel[topology, primaryParams, method]"
GetAnalyticalParams::usage = "GetAnalyticalParams[model]"
GetNumericalParams::usage = "GetNumericalParams[model]"
UpdatePrimaryParam::usage = "UpdatePrimaryParam[model, path, value]"
UpdateAnaliticalParam::usage = "UpdateAnaliticalParam[model, path, value]"
SetModelValue::usage = "SetModelValue[model, path, value] safely updates parameter";

$CurrentModel::usage = "Global reference to the active circuit model for substitution rules";


Begin["`Private`"];


$CurrentModel = Null;


(* ════════════════════════════════════════════════════════════════ *)
(* 		ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ПРАВИЛ ПОДСТАНОВКИ              *)
(* ════════════════════════════════════════════════════════════════ *)

SetModelValue[model_Association, path_List, value_] := 
  ($CurrentModel = ReplacePart[model, path -> value]);


(* Рекурсивный обход ассоциации для получения всех путей *)
getAllPaths[assoc_Association, currentPath_List : {}] := 
  Flatten[
    KeyValueMap[
      Function[{key, val}, 
        If[AssociationQ[val], 
          getAllPaths[val, Append[currentPath, key]], 
          {Append[currentPath, key]}
        ]
      ], 
      assoc
    ], 
    1
  ];

(* Построение правил подстановки с отложенным вычислением *)
BuildSubstitutionRules[primary_Association] := Module[
  {valuePaths, rules},
  
  (* Найти все пути, заканчивающиеся на "Value" *)
  valuePaths = Select[getAllPaths[primary], Last[#] === "Value" &];
  
  (* Построить правила: Symbol :> model["Primary"][путь к Value] *)
  rules = Map[
    Function[valuePath,
      Module[{symbolPath, symbol},
        (* Путь к Symbol: заменить "Value" на "Symbol" *)
        symbolPath = ReplacePart[valuePath, -1 -> "Symbol"];
        
        (* Извлечь символ из Primary *)
        symbol = primary[[Sequence @@ symbolPath]];
        
        (* Создать отложенное правило *)
        symbol :> Part[$CurrentModel,"Primary", Sequence @@ valuePath]
      ]
    ],
    valuePaths
  ];
  
  rules
];


(* ════════════════════════════════════════════════════════════════ *)
(* 		ГЕНЕРАЦИЯ PLACEHOLDER ИЗОБРАЖЕНИЯ                           *)
(* ════════════════════════════════════════════════════════════════ *)

GenerateCircuitImage[topology_Association] := Module[
  {nComponents, nNodes},
  
  nComponents = Length[topology["Components"]];
  nNodes = Length[topology["Nodes"]];
  
  Graphics[
    {
      LightGray,
      Rectangle[{0, 0}, {2, 1.5}],
      Black,
      Text[Style["Circuit Diagram", 14, Bold], {1, 1}],
      Text[Style[ToString[nComponents] <> " components", 11], {1, 0.6}],
      Text[Style[ToString[nNodes] <> " nodes", 11], {1, 0.3}]
    },
    ImageSize -> 200,
    PlotRange -> {{0, 2}, {0, 1.5}},
    ImagePadding -> 10
  ]
];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         			PRIMARY PARAMETERS                         	║ *)
(* ║    (Электрические компоненты и топология схемы)              	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


(* Объявление опций *)
Options[CreateCircuitModel] = {
  GroundNode -> Automatic,
  Method -> "HarmonicPerturbation" (* или "Diagonalization" *),
  CustomImage -> Automatic
};

  
CreateCircuitModel[components_List, opts : OptionsPattern[]] := 
  Module[{analytical, defaultPrimary, topology, method, gNode, 
  		  model,circuitImage},
    
    (*validated = ValidatePrimary[primaryParams, topology];
    If[validated === $Failed, Return[$Failed]];*)
    
	method = OptionValue[Method];
	gNode = If[OptionValue[GroundNode] === Automatic, 
	   (* берем макс. индекс узла *)
	   Max[Flatten[components[[All, {2, 3}]]]], 
	   OptionValue[GroundNode]
	];    
    
	(* Топология *)
	topology = CreateTopology[components, gNode];    
    
    (**)
    defaultPrimary = GenerateDefaultParameters[topology];
    
    (* Построить аналитические параметры (один раз) *)
    analytical = ComputeAnalyticalParams[topology, defaultPrimary, method];
    
    (* Изображение: либо пользовательское, либо placeholder *)
    circuitImage = If[OptionValue[CustomImage] === Automatic,
      GenerateCircuitImage[topology],
      OptionValue[CustomImage]
    ];
    
    (* Сборка *)
    model = <|
    	  "ModelVersion" -> "1.1",
    	  "Topology" -> topology,
      "Primary" -> defaultPrimary,
      "Image" -> circuitImage,
      "SubstitutionRules" -> {},
      "Analytical" -> analytical,
      "Numerical" -> <|
        "Method" -> method,
        "Cache" -> <||>,
        "IsDirty" -> True,
        "ComputationTime" -> Null,
        "ComputationStatus" -> <||>
      |>
    |>;

    (* Строим правила подстановки *)
    model["SubstitutionRules"] = BuildSubstitutionRules[defaultPrimary];
	
	(* Автоматически устанавливаем как текущую модель *)
    $CurrentModel = model;
	
	model
  ];


(* Вспомогательная функция: Генерация дефолтных параметров *)
GenerateDefaultParameters[topology_] := 
 Module[{compList, componentCounts},
  compList = topology["Components"];
  
  (* Счетчики для каждого типа компонента *)
  componentCounts = <|"Capacitor" -> 0, "JosephsonJunction" -> 0, "Inductor" -> 0|>;
  
  Association @ Map[
    Function[comp,
       Module[{type, name, params, symbols, defaultSymbols, count},
         type = comp[[1]];
         name = comp[[4]];
         symbols = If[Length[comp] >= 5, comp[[5]], <||>];
         
         (* Увеличить счетчик для этого типа *)
         componentCounts[type] = componentCounts[type] + 1;
         count = componentCounts[type];
         
         (* Генерация дефолтных символов с индексами *)
         defaultSymbols = Switch[type,
           "Capacitor",
           <|"C" -> Subscript[C, count]|>,
           
           "JosephsonJunction",
           <|"EJ" -> Subscript[EJ, count], "CJ" -> Subscript[CJ, count]|>,
           
           "Inductor",
           <|"L" -> Subscript[L, count]|>,
           
           _, <||>
         ];
         
         (* Объединить пользовательские и дефолтные символы *)
         symbols = Join[defaultSymbols, symbols];
         
         (* Логика выбора параметров в зависимости от типа *)
         params = Switch[type,
           
           "Capacitor",
           <|
             "Type" -> "Capacitor",
             "C" -> <|
               "Value" -> 10.*^-15, 
               "Symbol" -> symbols["C"],
               "Min" -> 1.*^-15, 
               "Max" -> 100.*^-15, 
               "Step" -> 1.*^-15, 
               "Interactive" -> True
             |>
           |>,
           
           "JosephsonJunction",
           <|
             "Type" -> "JosephsonJunction",
             "EJ" -> <|
               "Value" -> 15.*^9, 
               "Symbol" -> symbols["EJ"],
               "Min" -> 1.*^9, 
               "Max" -> 50.*^9, 
               "Step" -> 0.1*^9, 
               "Interactive" -> True
             |>,
             "CJ" -> <|
               "Value" -> 2.*^-15, 
               "Symbol" -> symbols["CJ"],
               "Min" -> 0.1*^-15, 
               "Max" -> 10.*^-15, 
               "Step" -> 0.1*^-15, 
               "Interactive" -> True
             |>
           |>,
           
           "Inductor",
           <|
             "Type" -> "Inductor",
             "L" -> <|
               "Value" -> 10.*^-9, 
               "Symbol" -> symbols["L"],
               "Min" -> 0.1*^-9, 
               "Max" -> 100.*^-9, 
               "Step" -> 0.1*^-9, 
               "Interactive" -> True
             |>
           |>,
           
           _, 
           <|"Type" -> "Generic", "Val" -> <|"Value" -> 0.|>|>
         ];
         
         name -> params
       ]
    ],
    compList
  ]
 ];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         			ANALYTICAL PARAMETERS                      	║ *)
(* ║  (Гамильтониан, матрицы, представления - символическое)      	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


ComputeAnalyticalParams[topology_, primaryParams_, method_] := 
 Module[{lagrangian, capMatrix, hamiltonian},
  
  (* Строим лагранжиан (временно, не сохраняем) *)
  lagrangian = BuildLagrangian[topology, primaryParams];
  
  (* Ёмкостная матрица *)
  capMatrix = BuildCapacitanceMatrix[lagrangian, topology];
  
  (* Гамильтониан *)
  hamiltonian = BuildHamiltonian[lagrangian, capMatrix, topology];
  
  (* Возвращаем структуру *)
  <|
    "CapacitanceMatrix" -> capMatrix,
    "Hamiltonian" -> hamiltonian
  |>
 ];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         			NUMERICAL PARAMETERS                        ║ *)
(* ║   (Численные вычисления для DynamicModule с кэшированием)      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


ComputeNumericalHarmonicPerturbation[model_Association] := Module[
    {
       analytical = model["Analytical"],
       primary = model["Primary"],
       cache = <||>
     },
    
    (* ============================================ *)
    (* FAST: Быстрые вычисления (вычисляем сразу) *)
    (* ============================================ *)
    
    (* Собственные значения *)
    cache["Eigenvalues"] = <|
        "State" -> "Ready",
        "Value" -> Eigenvalues[analytical["HamiltonianFull"]]
      |>;
    
    (* T1 lifetime *)
    cache["T1Lifetime"] = <|
        "State" -> "Ready",
        "Value" -> ComputeT1Fast[analytical, primary]
      |>;
    
    (* ============================================ *)
    (* LAZY: Медленные графики (откладываем) *)
    (* ============================================ *)
    
    (* Спектр с высокой точностью *)
    cache["PlotSpectrum"] = <|
        "State" -> "Lazy",
        "Thunk" -> Function[{m},
            Module[{evals},
               (* При первом вызове: 
       			достать быстрые eigenvalues из кэша *)
               evals = GetNumericalQuantity[m, "Eigenvalues"];
               (* Построить граф с высокой точностью *)
               
       PlotSpectrumHighResolution[evals, m["Primary"]["Elements"]]
             ]
          ]
      |>;
    
    (* График распада T1 *)
    cache["PlotDecay"] = <|
        "State" -> "Lazy",
        "Thunk" -> Function[{m},
            Module[{t1, evals},
               t1 = GetNumericalQuantity[m, "T1Lifetime"];
               evals = GetNumericalQuantity[m, "Eigenvalues"];
               
       PlotDecayHighResolution[t1, evals, m["Primary"]["Elements"]]
             ]
          ]
      |>;
    
    cache
  ];



(*вычисляет все*)
GetNumericalParams[model_Association] := Module[{
  analytical = model["Analytical"],
  numerical = model["Numerical"]
},
  (* Проверить, нужен ли пересчёт *)
  If[numerical["IsDirty"],
    numerical["Cache"] = Switch[numerical["Method"],
      "HarmonicPerturbation",
      ComputeNumericalHarmonicPerturbation[model],
      
      "Diagonalization",
      ComputeNumericalDiagonalization[model],
      
      _,
      $Failed
    ];
    numerical["IsDirty"] = False;
    numerical["ComputationTime"] = Now
  ];
  
  numerical["Cache"]
];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                  УПРАВЛЕНИЕ И КЭШИРОВАНИЕ                      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


(* Извлечь значение из записи кэша, вычисляя если нужно *)
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


(* Получить одно численное значение по ключу *)
GetNumericalQuantity[model_Association, key_String] := Module[
    {num, entry},
    
    num = model["Numerical"];
    
    (* Шаг 1: Если кэш грязный, сначала его пересчитать *)
    If[num["IsDirty"],
       num["Cache"] = Switch[num["Method"],
           "HarmonicPerturbation",
             ComputeNumericalHarmonicPerturbation[model],
           "Diagonalization",
             ComputeNumericalDiagonalization[model],
           _, $Failed
         ];
       num["IsDirty"] = False;
       num["ComputationTime"] = Now;
       model["Numerical"] = num;  (* Обновить model *)
     ];
    
    (* Шаг 2: Получить запрошенный ключ из кэша *)
  entry = Lookup[num["Cache"], key, Missing["UnknownKey"]];
    
    (* Шаг 3: Если ключа нет выдать ошибку *)
    If[entry === Missing["UnknownKey"],
       Message[GetNumericalQuantity::unknown, key];
       Return[$Failed]
     ];
    
    (* Шаг 4: Использовать GetCacheEntry для получения значения *)
    GetCacheEntry[entry, model]
  ];

GetNumericalQuantity::unknown = "Unknown key: `1`";


UpdateAnaliticalParam[model_Association, path_List, newValue_] := 
  Module[{updated},
    
    updated = model;
    
    (* Пересчитать аналитические параметры *)
    updated["Analytical"] = ComputeAnalyticalParams[
      updated["Primary"]["Topology"],
      updated["Primary"]["Elements"],
      updated["Numerical"]["Method"]
    ];
    
    updated
  ];


UpdatePrimaryParam[model_Association, path_List, newValue_] := 
  Module[{updated},
    
    updated = model;
    
    (* Обновить первичный параметр *)
    updated["Primary"] = 
      SetAtPath[model["Primary"], path, newValue];
    
    (* Отметить численный кэш как грязный *)
    updated["Numerical"]["IsDirty"] = True;
    
    updated
  ];
  
GetAnalyticalParams[model_Association] := model["Analytical"]

(* Удобный доступ ко всем параметрам *)
GetAllParams[model_Association] := <|
  "Primary" -> model["Primary"],
  "Analytical" -> model["Analytical"],
  "Numerical" -> GetNumericalParams[model]
|>;

End[];
EndPackage[];