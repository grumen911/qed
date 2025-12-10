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

Begin["`Private`"];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         УРОВЕНЬ 1: PRIMARY PARAMETERS                         	║ *)
(* ║    (Электрические компоненты и топология схемы)              	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

CreateCircuitModel[components_List, opts : OptionsPattern[]] := 
  CreateCircuitModel[components, Automatic, opts];
  
CreateCircuitModel[components_List, groundNode_,
                   method_String : "Diagonalization", opts : OptionsPattern[]] := 
  Module[{analytical, defaultPrimary, topology},
    
    (*validated = ValidatePrimary[primaryParams, topology];
    If[validated === $Failed, Return[$Failed]];*)
    
	(* Топология *)
	topology = CreateTopology[components, groundNode];    
    
    (**)
    defaultPrimary = GenerateDefaultParameters[topology];
    
    (* Построить аналитические параметры (один раз) *)
    analytical = ComputeAnalyticalParams[topology, primaryParams, method];
    
    (* Сборка *)
    <|
    	  "ModelVersion" -> "1.1",
    	  "topology" -> topology,
      "Primary" -> defaultPrimary,
      "Analytical" -> analytical,
      "Numerical" -> <|
        "Method" -> method,
        "Cache" -> <||>,
        "IsDirty" -> True,
        "ComputationTime" -> Null,
        "ComputationStatus" -> <||>
      |>
    |>

  ]


(* Вспомогательная функция: Генерация дефолтных параметров *)
GenerateDefaultParameters[topology_] := 
 Module[{compList},
  compList = topology["Components"];
  
  Association @ Map[
    Function[comp,
       Module[{type, name, params},
         type = comp[[1]];
         name = comp[[4]]; (* Тег/Имя компонента *)
         
         (* Логика выбора параметров в зависимости от типа *)
         params = Switch[type,
           
           "Capacitor",
           <|
             "Type" -> "Capacitor",
             "C" -> 	<|	"Value" -> 10.*^-15, 
             		  	"Min" -> 1.*^-15, 
             		  	"Max" -> 100.*^-15, 
             		  	"Step" -> 1.*^-15, 
             		  	"Interactive" -> True|>
           |>,
           
           "JosephsonJunction",
           <|
             "Type" -> "JosephsonJunction",
             "EJ" -> <|	"Value" -> 15.*^9, 
             			"Min" -> 1.*^9, 
             			"Max" -> 50.*^9, 
             			"Step" -> 0.1*^9, 
             			"Interactive" -> True|>,
             "CJ" -> <|	"Value" -> 2.*^-15, 
             			"Min" -> 0.1*^-15, 
             			"Max" -> 10.*^-15, 
             			"Step" -> 0.1*^-15, 
             			"Interactive" -> True|>
           |>,
           
           "Inductor",
           <|
             "Type" -> "Inductor",
             "L" -> <|	"Value" -> 10.*^-9, 
             			"Min" -> 0.1*^-9, 
             			"Max" -> 100.*^-9, 
             			"Step" -> 0.1*^-9, 
             			"Interactive" -> True|>
           |>,
           
           _, (* Неизвестный тип *)
           <|"Type" -> "Generic", "Val" -> <|"Value" -> 0.|>|>
         ];
         
         (* Возвращаем пару: ИмяКомпонента -> Параметры *)
         name -> params
       ]
    ],
    compList
  ]
 ];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         УРОВЕНЬ 2: ANALYTICAL PARAMETERS                      	║ *)
(* ║  (Гамильтониан, матрицы, представления - символическое)      	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


(* Определить граф зависимостей *)
dependencyGraph = {
  "HamiltonianFull" -> {},
  "CapacitanceMatrix" -> {"HamiltonianFull"},
  "InductanceMatrix" -> {"HamiltonianFull"},
  "Eigenvalues" -> {"HamiltonianFull"},
  "Anharmonicity" -> {"Eigenvalues"},
  "T1Lifetime" -> {"Anharmonicity", "CapacitanceMatrix"}
};

ComputeAnalyticalParams[topology_, primaryParams_, method_String] := 
  Module[{cache = <||> , compute, graph = Association[dependencyGraph]},
    
    compute[key_] := 
      cache[key] /; KeyExistsQ[cache, key];
    
    compute[key_] := (
      (* Сначала вычислить все зависимости *)
      Scan[compute, graph[key]];
      
      (* Затем вычислить сам параметр *)
      cache[key] = Switch[key,
        "HamiltonianFull",
        BuildHamiltonian[topology, primaryParams],
        
        "CapacitanceMatrix",
        BuildCapacitanceMatrix[primaryParams, topology, cache["HamiltonianFull"]],
        
        "InductanceMatrix",
        BuildInductanceMatrix[primaryParams, topology, cache["HamiltonianFull"]],
        
        "Eigenvalues",
        Eigenvalues[cache["HamiltonianFull"]],
        
        "Anharmonicity",
        ComputeAnharmonicity[cache["Eigenvalues"], primaryParams],
        
        "T1Lifetime",
        ComputeT1[cache["Anharmonicity"], cache["CapacitanceMatrix"]],
        
        _,
        $Failed
      ]
    );
    
    (* Запросить нужные результаты *)
    AssociationMap[compute, Union @ Flatten @ 
      graph[{"HamiltonianFull", "CapacitanceMatrix", "InductanceMatrix"}]]
  ]


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         УРОВЕНЬ 3: NUMERICAL PARAMETERS                        ║ *)
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
  ]



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
]


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
]


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
  ]

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
  ]


UpdatePrimaryParam[model_Association, path_List, newValue_] := 
  Module[{updated},
    
    updated = model;
    
    (* Обновить первичный параметр *)
    updated["Primary"] = 
      SetAtPath[model["Primary"], path, newValue];
    
    (* Отметить численный кэш как грязный *)
    updated["Numerical"]["IsDirty"] = True;
    
    updated
  ]
  
GetAnalyticalParams[model_Association] := model["Analytical"]

(* Удобный доступ ко всем параметрам *)
GetAllParams[model_Association] := <|
  "Primary" -> model["Primary"],
  "Analytical" -> model["Analytical"],
  "Numerical" -> GetNumericalParams[model]
|>

End[];
EndPackage[];