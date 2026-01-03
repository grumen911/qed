(*BeginPackage["QED`Model`", {"QED`CircuitTopology`","QED`Numeric`","QED`Analytic`"}];*)
BeginPackage["QED`Model`"];

Needs["QED`CircuitTopology`"];
Needs["QED`Numeric`"];
Needs["QED`Analytic`"];

CreateCircuitModel::usage = "CreateCircuitModel[topology, primaryParams, method]"
GetAnalyticalParams::usage = "GetAnalyticalParams[model]"
GetNumericalQuantity::usage = "GetNumericalQuantity[model, key]"
GetNumericalParams::usage = "GetNumericalParams[model]"
GetCacheEntry::usage = "GetCacheEntry[cacheEntry, model]"
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
BuildSubstitutionRules[primary_Association, topology_Association] := Module[
  {valuePaths, primaryRules, constantRules, equilibriumRules, nodes, minSymbols},
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* PRIMARY PARAMETERS (C, EJ, L, Φₑₓₜ)                              *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Найти все пути, заканчивающиеся на "Value" *)
  valuePaths = Select[getAllPaths[primary], Last[#] === "Value" &];
  
  (* Построить правила: Symbol :> model["Primary"][путь к Value] *)
  primaryRules = Map[
    Function[valuePath,
      Module[{symbolPath, symbol},
        symbolPath = ReplacePart[valuePath, -1 -> "Symbol"];
        symbol = primary[[Sequence @@ symbolPath]];
        
        symbol :> (Part[$CurrentModel,"Primary", Sequence @@ valuePath] * 
           If[symbol === QED`$PhiExt, QED`$Phi0Value, 1])			
      ]
    ],
    valuePaths
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* PHYSICAL CONSTANTS (Φ₀, ℏ, e, kB, ...)                          *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  constantRules = {
    QED`$Phi0 :> QED`$Phi0Value
  };
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* EQUILIBRIUM FLUXES (φ_min)                                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  (* Получить узлы из переданной topology *)
  nodes = Cases[
    topology["Nodes"], 
    Except[topology["GroundNode"]]
  ];
  
  (* Создать символы φ_min *)
  minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
  
  (* Отложенные правила: φ_min :> значение из кэша *)
  equilibriumRules = Table[
    With[{idx = i},
      minSymbols[[idx]] :> Part[$CurrentModel, "Numerical", "Cache", "EquilibriumFluxes", "Value", idx, 2]
    ],
    {i, Length[minSymbols]}
  ];
  
  Join[primaryRules, constantRules, equilibriumRules]
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
    model["SubstitutionRules"] = BuildSubstitutionRules[defaultPrimary, topology];
	
	(* Автоматически устанавливаем как текущую модель *)
    $CurrentModel = model;
	
	model
  ];


(* Вспомогательная функция: Генерация дефолтных параметров *)
GenerateDefaultParameters[topology_] := 
 Module[{compList, componentCounts, fluxLoops, primaryParams},
 	
  compList = topology["Components"];
  fluxLoops = topology["GraphStructure"]["fluxLoops"];
  
  (* Счетчики для каждого типа компонента *)
  componentCounts = <|"Capacitor" -> 0, "JosephsonJunction" -> 0, "Inductor" -> 0|>;
  
  primaryParams = Association @ Map[
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
		   <|"C" -> Subscript[QED`$CapacitanceSymbol, count]|>,
		  
		   "JosephsonJunction",
		   <|"EJ" -> Subscript[QED`$JosephsonEnergySymbol, count],
		    "CJ" -> Subscript[QED`$JosephsonCapacitanceSymbol, count]|>,
		  
		   "Inductor",
		   <|"L" -> Subscript[QED`$InductanceSymbol, count]|>,
		  
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
               "Value" -> 9.94*^-24, 
               "Symbol" -> symbols["EJ"],
               "Min" -> 1.*^-24, 
               "Max" -> 50.*^-24, 
               "Step" -> 0.1*^-24, 
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
  ];
  
  (* Добавить параметры внешних магнитных потоков *)
	If[fluxLoops =!= <||>,
	  (* Создать только ОДИН параметр внешнего потока *)
	  primaryParams["Fext"] = <|
	    "Type" -> "ExternalFlux",
	    "Fext" -> <|
	      "Value" -> 0.3,
	      "Symbol" -> QED`$PhiExt,
	      "Min" -> -0.5,
	      "Max" -> 0.5,
	      "Step" -> 0.01,
	      "Interactive" -> True,
	      "Unit" -> "Φ₀"  (* Φ₀ *)
	    |>
	  |>
	];

  primaryParams
 ];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         			ANALYTICAL PARAMETERS                      	║ *)
(* ║  (Гамильтониан, матрицы, представления - символическое)      	║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


ComputeAnalyticalParams[topology_, primaryParams_, method_] := 
 Module[{lagrangian, capMatrix, indMatrix, hamiltonian, harmonicHamiltonian,
 		 potentialGradient},
  
  lagrangian = BuildLagrangian[topology, primaryParams];
  capMatrix = BuildCapacitanceMatrix[lagrangian, topology];
  hamiltonian = BuildHamiltonian[lagrangian, capMatrix, topology];
  harmonicHamiltonian = BuildHarmonicHamiltonian[hamiltonian, topology];
  
  (* Индуктивная матрица (обратная) *)
  indMatrix = BuildInductanceMatrix[hamiltonian, topology];
  
  (*Градиент потенциала для поиска равновесия *)
  potentialGradient = BuildPotentialGradient[hamiltonian, topology];

  <|
    "CapacitanceMatrix" -> capMatrix,
    "Hamiltonian" -> hamiltonian,
    "HarmonicHamiltonian" -> harmonicHamiltonian,
    "InductanceMatrix" -> indMatrix,
    "PotentialGradient" -> potentialGradient
  |>
 ];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         			NUMERICAL PARAMETERS                              ║ *)
(* ║   (Численные вычисления для DynamicModule с кэшированием)      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


ComputeNumericalHarmonicPerturbation[model_Association] := Module[
    {analytical, topology, subRules, cache, capNum, hamNum, indNum,
     equilibriumFluxes, equilibriumFluxesContinuation,
     equilibriumPoints, subRulesWithoutPhiExt},
     
    analytical = model["Analytical"];
    subRules = model["SubstitutionRules"];
    topology = model["Topology"];
    cache = <||>;

    (* ════════════════════════════════════════════════════════════════ *)
    (* Шаг 1: Численный гамильтониан + производные для continuation     *)
    (* ════════════════════════════════════════════════════════════════ *)

    (* Полный гамильтониан для NMinimize *)
    hamNum = analytical["Hamiltonian"] /. subRules;
    cache["HamiltonianNumerical"] = <|"State" -> "Ready", "Value" -> hamNum|>;

    (* Частичный гамильтониан (без Φext) для continuation *)
    subRulesWithoutPhiExt = DeleteCases[subRules, QED`$PhiExt :> _];
    hamNumPartial = analytical["Hamiltonian"] /. subRulesWithoutPhiExt;

    cache["HamiltonianNumericalPartial"] = <|
      "State" -> "Ready", 
      "Value" -> hamNumPartial
    |>;

    (* Предвычисление производных для continuation *)
    Module[{nodes, fluxVars, potential, potentialRescaled, phi0},
      nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
      fluxVars = Subscript[QED`$FluxSymbol, #] & /@ nodes;
      phi0 = QED`$Phi0Value;
      
      (* Потенциал: U(φ) = H(q=0, φ) *)
      potential = hamNumPartial /. Subscript[QED`$ChargeSymbol, _] -> 0;
      
      (* Обезразмерить: U(φ) → U(φ̃ * Φ₀) *)
      potentialRescaled = potential /. Thread[fluxVars -> fluxVars * phi0];
      
      (* Символьное дифференцирование (один раз!) *)
      cache["ContinuationDerivatives"] = <|
        "State" -> "Ready",
        "Gradient" -> Simplify@(D[potentialRescaled, #] & /@ fluxVars),
        "Hessian" -> Simplify@D[potentialRescaled, {fluxVars, 2}],
        "FluxVars" -> fluxVars
      |>;
    ];


    (* ════════════════════════════════════════════════════════════ *)
    (* Шаг 2: Равновесные потоки (ПЕРЕНЕСЛИ СЮДА!)                 *)
    (* ════════════════════════════════════════════════════════════ *)
    
      equilibriumFluxes = FindPotentialMinimum[hamNum, topology, subRules];

      equilibriumFluxesContinuation = QED`Numeric`FindPotentialMinimumContinuation[
        cache["ContinuationDerivatives"]["Gradient"],
        cache["ContinuationDerivatives"]["Hessian"],
        cache["ContinuationDerivatives"]["FluxVars"],
        topology,
        QED`$PhiExt /. subRules
      ];

      cache["EquilibriumFluxes1231"] = <|"State" -> "Ready", "Value" -> equilibriumFluxes|>;
      cache["EquilibriumFluxes"] = <|
        "State" -> "Ready", 
        "Value" -> equilibriumFluxesContinuation
      |>;

    (* Равновесные точки (LAZY) *)
    cache["EquilibriumPoints"] = <|
      "State" -> "Lazy",
      "Thunk" -> Function[{m},
        FindEquilibriumPoints[
          hamNum,
          analytical["PotentialGradient"],
          topology,
          subRules
        ]
      ]
    |>;

    (* ОБНОВИТЬ $CurrentModel чтобы правила подстановки работали! *)
    $CurrentModel = ReplacePart[$CurrentModel, {"Numerical", "Cache"} -> cache];
    
    (* ════════════════════════════════════════════════════════════ *)
    (* Шаг 3: Остальные матрицы (ТЕПЕРЬ с φ_min!)                   *)
    (* ════════════════════════════════════════════════════════════ *)
    
    {capNum, indNum} = {
      analytical["CapacitanceMatrix"] /. subRules,
      analytical["InductanceMatrix"] /. subRules
    };
    
    cache["CapacitanceMatrixNumerical"] = <|"State" -> "Ready", "Value" -> capNum|>;
    cache["InductanceMatrixInverseNumerical"] = <|"State" -> "Ready", "Value" -> indNum|>;
    
    (* ════════════════════════════════════════════════════════════ *)
    (* Обратные матрицы                                             *)
    (* ════════════════════════════════════════════════════════════ *)
    
    If[Det[capNum] != 0,
      cache["InverseCapacitanceMatrix"] = <|"State" -> "Ready", "Value" -> Inverse[capNum]|>,
      cache["InverseCapacitanceMatrix"] = <|"State" -> "Failed", "Error" -> "Singular matrix"|>
    ];
    
    If[Det[indNum] != 0,
      cache["InductanceMatrixNumerical"] = <|"State" -> "Ready", "Value" -> Inverse[indNum]|>,
      cache["InductanceMatrixNumerical"] = <|"State" -> "Failed", "Error" -> "Singular inductance matrix"|>
    ];
    
    (* ════════════════════════════════════════════════════════════ *)
    (* Плазмонные частоты (собственные моды)                        *)
    (* ════════════════════════════════════════════════════════════ *)
    
    If[cache["InverseCapacitanceMatrix"]["State"] === "Ready" && 
       cache["InductanceMatrixNumerical"]["State"] === "Ready",
      
      Module[{invC, invL, result},
        invC = cache["InverseCapacitanceMatrix"]["Value"];
        invL = cache["InductanceMatrixInverseNumerical"]["Value"];
        
        result = ComputeNormalModeFrequencies[invC, invL];
        
        cache["PlasmonFrequencies"] = <|
          "State" -> "Ready",
          "Value" -> result["Frequencies"],           (* Для GetNumericalQuantity *)
          "Frequencies" -> result["Frequencies"],      (* Явный доступ *)
          "IsStable" -> result["IsStable"],
          "NumUnstableModes" -> result["NumUnstableModes"]
        |>
      ],
      
      (* Если матрицы сингулярные *)
      cache["PlasmonFrequencies"] = <|"State" -> "Failed", "Error" -> "Singular matrices"|>
    ];

    (* ════════════════════════════════════════════════════════════════ *)
    (* Plasmon Frequencies vs Flux (Lazy)                              *)
    (* ════════════════════════════════════════════════════════════════ *)

    cache["PlasmonFrequenciesVsFlux"] = <|
      "State" -> "Lazy",
      "Thunk" -> Function[{m},
        QED`Numeric`PlasmonFrequenciesVsFlux[m]
      ]
    |>;
    
	(* ════════════════════════════════════════════════════════════════ *)
	(* Harmonic mode diagonalization (READY)                            *)
	(* ════════════════════════════════════════════════════════════════ *)
	
	If[cache["InverseCapacitanceMatrix"]["State"] === "Ready" && 
	   cache["InductanceMatrixInverseNumerical"]["State"] === "Ready",
	  
	  Module[{invC, invL, diag},
	    invC = cache["InverseCapacitanceMatrix"]["Value"];
	    invL = cache["InductanceMatrixInverseNumerical"]["Value"];
	    
	    diag = DiagonalizeHarmonicHamiltonian[invC, invL];
	    
	    cache["HarmonicDiagonalization"] = <|
	      "State" -> "Ready",
	      "Value" -> diag
	    |>
	  ],
	  
	  (* Если матрицы Failed *)
	  cache["HarmonicDiagonalization"] = <|
	    "State" -> "Failed", 
	    "Error" -> "Capacitance or inductance matrix unavailable"
	  |>
	];
    
    (* ════════════════════════════════════════════════════════════ *)
    (* Lazy кэш (пример для PlotTest)                              *)
    (* ════════════════════════════════════════════════════════════ *)
    
    cache["PlotTest"] = <|
      "State" -> "Lazy",
      "Thunk" -> Function[{m},
        Module[{invC, element},
          invC = GetNumericalQuantity[m, "InverseCapacitanceMatrix"];
          If[invC === $Failed, $Failed,
            element = invC[[1, 1]];
            Plot[element * Sin[x], {x, 0, 1},
              PlotLabel -> Row[{"Test: Sin(", ScientificForm[element], " × x × 10¹⁵)"}],
              PlotTheme -> "Scientific",
              ImageSize -> 400
            ]
          ]
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
      
      $CurrentModel = ReplacePart[$CurrentModel, "Numerical" -> num];  (* Обновить model *)
     ];
    
    (* Шаг 2: Получить запрошенный ключ из кэша *)
  	entry = Lookup[num["Cache"], key, Missing["UnknownKey"]];
    
    If[entry === Missing["UnknownKey"],
       Message[GetNumericalQuantity::unknown, key];
       Return[$Failed]
     ];
    
    (* Шаг 3: Использовать GetCacheEntry для получения значения *)
    GetCacheEntry[entry, $CurrentModel]
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