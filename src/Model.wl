BeginPackage["QED`Model`"];

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


CreateCircuitModel[topology_Association, primaryParams_Association, 
                   method_String : "Diagonalization"] := 
  Module[{validated, analytical, structure},
    
    validated = ValidatePrimary[primaryParams, topology];
    If[validated === $Failed, Return[$Failed]];
    
    (* Построить аналитические параметры (один раз) *)
    analytical = ComputeAnalyticalParams[topology, primaryParams, method];
    
    (* Основная структура *)
    structure = <|
      
      (* === УРОВЕНЬ 1: Первичные параметры (электрические) === *)
      "Primary" -> <|
        "Elements" -> primaryParams["Elements"],
        "ExternalControl" -> primaryParams["ExternalControl", <||>],
        "Topology" -> topology,
        "Metadata" -> <|
          "CircuitType" -> topology["Type"],
          "CreationTime" -> Now
        |>
      |>,
      
      (* === УРОВЕНЬ 2: Аналитические параметры (символические) === *)
      "Analytical" -> analytical,
      
      (* === УРОВЕНЬ 3: Численные параметры (кэшируемые) === *)
      "Numerical" -> <|
        "Method" -> method,
        "Cache" -> <||>,
        "IsDirty" -> True,
        "ComputationTime" -> Null,
        "ComputationStatus" -> <||>
      |>
    |>;
    
    structure
  ]


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
(* ║         УРОВЕНЬ 3: NUMERICAL PARAMETERS                       	║ *)
(* ║   (Численные вычисления для DynamicModule с кэшированием)     	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


GetNumericalParams[model_Association] := Module[{
  analytical = model["Analytical"],
  numerical = model["Numerical"]
},
  (* Проверить, нужен ли пересчёт *)
  If[numerical["IsDirty"],
    numerical["Cache"] = Switch[numerical["Method"],
      "Diagonalization",
      ComputeNumerical_Diagonalization[model],
      
      "HarmonicPerturbation",
      ComputeNumerical_HarmonicPerturbation[model],
      
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
           "Diagonalization",
             ComputeNumerical_Diagonalization[model],
           "HarmonicPerturbation",
             ComputeNumerical_HarmonicPerturbation[model],
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