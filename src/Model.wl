BeginPackage["QED`Model`"];

Needs["QED`CircuitTopology`"];
Needs["QED`Numeric`"];
Needs["QED`Analytic`"];
Needs["QED`Scattering`"];

CreateCircuitModel::usage = "CreateCircuitModel[topology, primaryParams, method]"
GetAnalyticalParams::usage = "GetAnalyticalParams[model]"
GetNumericalQuantity::usage = "GetNumericalQuantity[model, key]"
GetNumericalParams::usage = "GetNumericalParams[model]"
GetCacheEntry::usage = "GetCacheEntry[cacheEntry, model]"
UpdateAnaliticalParam::usage = "UpdateAnaliticalParam[model, path, value]"
SetModelValue::usage = "SetModelValue[model, path, value] safely updates parameter";


SavePreset::usage = "SavePreset[model, name] saves the current Primary parameters into the Presets registry under the given name. Returns updated model.";
LoadPreset::usage = "LoadPreset[model, name] loads Primary parameters from the specified preset. Returns updated model with IsDirty=True.";
DeletePreset::usage = "DeletePreset[model, name] removes a preset from the registry.";
GetPresetNames::usage = "GetPresetNames[model] returns a list of available preset names.";
MergePresets::usage = "MergePresets[model, newPresets] merges an association of presets into the model's registry.";

GetWaveFunction::usage = "GetWaveFunction[model, quantumNumbers] returns the analytical wavefunction \
Psi[phi1, phi2, ...] for the specified state {n1, n2, ...} in physical flux coordinates.";
UpdateModelWithRules::usage = "UpdateModelWithRules[model, rules] updates the model's SubstitutionRules \
(at root) and manually populates the numerical cache with matrices and diagonalization data, \
setting IsDirty->False. This allows skipping the expensive FindPotentialMinimum step during sweeps.";

GetParameterVector::usage = "GetParameterVector[model] returns a sorted PackedArray of Reals representing \
the model's primary parameters for JIT compilation.";

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
    QED`$Phi0 -> QED`$Phi0Value,
    QED`$hbar -> QED`$hbarValue,
    QED`$e -> QED`$eValue
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

GetParameterVector[modelAssoc_] := Module[
  {allRules, paramRules, evaluatedRules, sortedRules, valuesVector},
  
  (* Извлекаем базовые правила подстановки *)
  allRules = modelAssoc["SubstitutionRules"];
  
  (* Фильтруем физические константы и потоки равновесия (min) *)
  paramRules = Select[allRules, 
    Function[ruleItem, 
      Not[StringContainsQ[ToString[ruleItem[[1]]], "min"]] &&
      Not[MemberQ[{QED`$Phi0, QED`$hbar, QED`$e}, ruleItem[[1]]]]
    ]
  ];
  
  (* Раскрываем RuleDelayed (:>) и принудительно переводим в числа *)
  evaluatedRules = Map[
    Function[ruleItem, ruleItem[[1]] -> N[ReleaseHold[ruleItem[[2]]]]], 
    paramRules
  ];
  
  (* Жесткая сортировка ключей по алфавиту *)
  sortedRules = SortBy[evaluatedRules, Function[ruleItem, ToString[ruleItem[[1]]]]];
  
  (* Извлекаем только значения *)
  valuesVector = Map[Last, sortedRules];
  
  (* Упаковываем в плоский вектор для CompiledFunction *)
  Developer`ToPackedArray[valuesVector, Real]
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
      "Presets" -> <||>,
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
 		 potentialGradient, currentOp, voltageOperatorsSym, nodes, scattering, potential},
  
  lagrangian = BuildLagrangian[topology, primaryParams];
  capMatrix = BuildCapacitanceMatrix[lagrangian, topology];
  hamiltonian = BuildHamiltonian[lagrangian, capMatrix, topology];
  harmonicHamiltonian = BuildHarmonicHamiltonian[hamiltonian, topology];
  currentOp = QED`Analytic`BuildCurrentOperator[hamiltonian, topology];
  
  nodes = Cases[topology["Nodes"], Except[topology["GroundNode"]]];
  voltageOperatorsSym = AssociationMap[
    Function[n, QED`Analytic`BuildVoltageOperator[hamiltonian, topology, n]],
    nodes
  ];
  
  (* Индуктивная матрица (обратная) *)
  indMatrix = BuildInductanceMatrix[hamiltonian, topology];
  
  (*Градиент потенциала для поиска равновесия *)
  potentialGradient = BuildPotentialGradient[hamiltonian, topology];

  scattering = QED`Scattering`BuildSymbolicScattering[topology, primaryParams];

  <|
    "CapacitanceMatrix" -> capMatrix,
    "Hamiltonian" -> hamiltonian,
    "HarmonicHamiltonian" -> harmonicHamiltonian,
    "InductanceMatrix" -> indMatrix,
    "PotentialGradient" -> potentialGradient,
    "Potential" -> hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0,
    "CurrentOperator" -> currentOp,
    "VoltageOperators" -> voltageOperatorsSym,
    "Scattering" -> scattering
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
    (* Шаг 2.5: S-матрица (Semi-Symbolic) и Эффективные индуктивности *)
    (* ════════════════════════════════════════════════════════════ *)
    Module[{effRules, symS, sRaw, sNum, fullRules},
        (* 1. Считаем L_eff используя ТОЛЬКО ЧТО найденные потоки *)
        effRules = QED`Scattering`GetEffectiveInductances[model, equilibriumFluxesContinuation];
        
        cache["EffectiveInductances"] = <|
            "State" -> "Ready", 
            "Value" -> effRules
        |>;

        (* 2. Формируем полусимвольную S-матрицу (числа + s) *)
        If[effRules =!= $Failed,
            symS = analytical["Scattering"];
            sRaw = symS["SMatrixRaw"]; 
            
            (* Объединяем статические параметры (C, L_linear) и динамические (L_eff) *)
            fullRules = Join[subRules, effRules];
            
            sNum = sRaw /. fullRules;
            
            cache["SMatrixNumerical"] = <|
                "State" -> "Ready", 
                "Value" -> sNum, (* Матрица чисел, зависящая от s *)
                "FrequencyVariable" -> symS["FrequencyVariable"]
            |>;
        ,
            cache["SMatrixNumerical"] = <|"State" -> "Failed", "Error" -> "Could not calc effective inductances"|>
        ];
    ];

    (* ════════════════════════════════════════════════════════════ *)
    (*         Шаг 2a: Численный оператор тока                      *)
    (* ════════════════════════════════════════════════════════════ *)

    Module[{opSym, opNum},
        (* Извлекаем символьный оператор (если он был посчитан в Analytic) *)
        opSym = Lookup[analytical, "CurrentOperator", 0];
        
        (* Подставляем числа: параметры EJ, C, PhiExt И найденные phi_min *)
        opNum = opSym /. subRules;
        
        cache["CurrentOperatorNumerical"] = <|
            "State" -> "Ready", 
            "Value" -> opNum
        |>;
    ];  
    (* Шаг 2b: Численные операторы напряжения *)
    Module[{opsSym, opsNum},
        opsSym = Lookup[analytical, "VoltageOperators", <||>];
        
        (* Подставляем параметры (C, L, ...) и равновесные потоки *)
        opsNum = opsSym /. subRules;
        
        cache["VoltageOperatorsNumerical"] = <|
            "State" -> "Ready", 
            "Value" -> opsNum
        |>;
    ];

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
       cache["InductanceMatrixInverseNumerical"]["State"] === "Ready",
      
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

  
GetAnalyticalParams[model_Association] := model["Analytical"]


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║             API: WAVEFUNCTIONS                                 ║ *)
(* ╚════════════════════════════════════════════════════════════════ *)

GetWaveFunction[model_Association, quantumNumbers_List] := 
 Module[{diagData, frequencies, effCaps, transform, topology, subRules, psiSymbolic},
  
  (* 1. Получить данные диагонализации *)
  diagData = GetNumericalQuantity[model, "HarmonicDiagonalization"];
  If[diagData === $Failed, Return[$Failed]];
  
  frequencies = diagData["NormalModeFrequencies"];
  effCaps = diagData["EffectiveCapacitances"];
  transform = diagData["FluxTransform"];
  
  topology = model["Topology"];
  subRules = model["SubstitutionRules"];
  
  (* 2. Построить символьное выражение с подставленными коэффициентами *)
  psiSymbolic = QED`Analytic`BuildHarmonicWavefunction[
    topology,
    frequencies,
    effCaps,
    transform,
    quantumNumbers
  ];
  
  (* 3. Подставить значения равновесных потоков (φ_min), чтобы получить чистую функцию от φ *)
  (* Используем subRules, которые содержат правила для minSymbols *)
  psiSymbolic //. subRules
 ];

UpdateModelWithRules[model_Association, rules_List] := Module[
    {
        analytical, capNum, indNum, invCap, invInd, diag, 
        newCache, existingCache, newModel, currentOpNum,
        voltageOpsNum
    },

    analytical = model["Analytical"];
    existingCache = model["Numerical"]["Cache"];

    (* 1. Вычисляем матрицы (быстрая подстановка) *)
    capNum = analytical["CapacitanceMatrix"] /. rules;
    indNum = analytical["InductanceMatrix"] /. rules; (* Это L^-1 ! *)
    currentOpNum = Lookup[analytical, "CurrentOperator", 0] /. rules;
    voltageOpsNum = Lookup[analytical, "VoltageOperators", <||>] /. rules;

    (* 2. Обращаем матрицы *)
    (* invCap = C^-1 *)
    invCap = If[Det[capNum] != 0, Inverse[capNum], $Failed];
    (* invInd = L (прямая индуктивность) - нужна для кэша, но не для диагонализации *)
    invInd = If[Det[indNum] != 0, Inverse[indNum], $Failed];
    
    (* 3. Диагонализация *)
    (* ИСПРАВЛЕНИЕ: Передаем (C^-1, L^-1), то есть (invCap, indNum) *)
    diag = If[MatrixQ[invCap] && MatrixQ[indNum],
        DiagonalizeHarmonicHamiltonian[invCap, indNum],
        $Failed
    ];

    effRules = QED`Scattering`GetEffectiveInductances[model, rules];

    (* Рассчитываем численную S-матрицу (numbers + s) *)
    sMatrixNum = If[effRules =!= $Failed,
        analytical["Scattering"]["SMatrixRaw"] /. Join[rules, effRules],
        $Failed
    ];

    (* 4. Формируем обновления для кэша *)
    newCache = <|
        "CapacitanceMatrixNumerical"       -> <|"State" -> "Ready", "Value" -> capNum|>,
        "InductanceMatrixInverseNumerical" -> <|"State" -> "Ready", "Value" -> indNum|>, (* L^-1 *)
        "InverseCapacitanceMatrix"         -> <|"State" -> "Ready", "Value" -> invCap|>, (* C^-1 *)
        "InductanceMatrixNumerical"        -> <|"State" -> "Ready", "Value" -> invInd|>, (* L *)
        "CurrentOperatorNumerical"         -> <|"State" -> "Ready", "Value" -> currentOpNum|>,
        "VoltageOperatorsNumerical"        -> <|"State" -> "Ready", "Value" -> voltageOpsNum|>,
        "HarmonicDiagonalization"          -> <|"State" -> "Ready", "Value" -> diag|>,

        "EffectiveInductances"             -> <|"State" -> "Ready", "Value" -> effRules|>,
        "SMatrixNumerical"                 -> <|
                                                "State" -> "Ready", 
                                                "Value" -> sMatrixNum,
                                                "FrequencyVariable" -> analytical["Scattering"]["FrequencyVariable"]
                                              |>,

        "PlasmonFrequencies" -> <|
            "State" -> "Ready", 
            "Value" -> Sort[If[diag === $Failed, $Failed, diag["NormalModeFrequencies"]]]
        |>,
        "EquilibriumFluxes" -> <|
            "State" -> "Ready",
            "Value" -> FilterRules[rules, Subscript[QED`$FluxSymbol, "min", _]]
        |>
    |>;

    (* 5. Собираем новую модель *)
    newModel = model;
    newModel["SubstitutionRules"] = rules;
    newModel["Numerical"]["Cache"] = Join[existingCache, newCache];
    newModel["Numerical"]["IsDirty"] = False;

    newModel
];

(* ════════════════════════════════════════════════════════════════ *)
(* PRESET MANAGEMENT SYSTEM                             *)
(* ════════════════════════════════════════════════════════════════ *)

SavePreset[model_Association, name_String] := 
  Module[{updatedModel},
    If[name === "", Return[model]]; (* Защита от пустого имени *)
    
    updatedModel = model;
    (* Сохраняем полную копию Primary (значения, лимиты, символы) *)
    updatedModel["Presets", name] = model["Primary"];
    
    updatedModel
  ];

LoadPreset[model_Association, name_String] := 
  Module[{updatedModel, presetData},
    (* Проверяем наличие пресета *)
    If[!KeyExistsQ[model["Presets"], name],
       Message[LoadPreset::nopreset, name];
       Return[model]
    ];
    
    presetData = model["Presets", name];
    updatedModel = model;
    
    (* Восстанавливаем Primary *)
    updatedModel["Primary"] = presetData;
    
    (* Критично: обновляем правила подстановки, так как Value изменились *)
    (* Примечание: BuildSubstitutionRules зависит от текущей model, но мы передаем данные явно *)
    (* В текущей архитектуре параметры подставляются через SubstitutionRules, 
       которые ссылаются на Primary. Но лучше сбросить кэш. *)
       
    updatedModel["Numerical", "IsDirty"] = True;
    
    updatedModel
  ];

LoadPreset::nopreset = "Preset '`1`' not found in the model.";

MergePresets[model_Association, newPresets_Association] := 
  Module[{updated},
    updated = model;
    (* Join[old, new] - ключи из new перезаписывают ключи из old, если совпадают *)
    updated["Presets"] = Join[model["Presets"], newPresets];
    updated
  ];

DeletePreset[model_Association, name_String] := 
  Module[{updatedModel},
    updatedModel = model;
    updatedModel["Presets"] = KeyDrop[model["Presets"], name];
    updatedModel
  ];

GetPresetNames[model_Association] := Keys[model["Presets"]];

(* Удобный доступ ко всем параметрам *)
GetAllParams[model_Association] := <|
  "Primary" -> model["Primary"],
  "Analytical" -> model["Analytical"],
  "Numerical" -> GetNumericalParams[model]
|>;

End[];
EndPackage[];