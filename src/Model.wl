BeginPackage["QED`Model`"];

Needs["QED`CircuitTopology`"];
Needs["QED`Numeric`"];
Needs["QED`Analytic`"];
Needs["QED`Scattering`"];

CreateCircuitModel::usage = "CreateCircuitModel[topology, primaryParams, method]";
GetAnalyticalParams::usage = "GetAnalyticalParams[model]";
GetNumericalQuantity::usage = "GetNumericalQuantity[model, key]";
GetCacheEntry::usage = "GetCacheEntry[cacheEntry, model]";
UpdateAnaliticalParam::usage = "UpdateAnaliticalParam[model, path, value]";
GetParameterRules::usage = "GetParameterRules[model] generates strict substitution rules for primary parameters on the fly.";
GetStaticRules::usage = "GetStaticRules[model] returns a strict list of rules for primary parameters and physical constants.";

RegisterModel::usage = "RegisterModel[model] stores the model in the global registry and returns its UUID.";
GetModel::usage = "GetModel[id] retrieves a model from the global registry by its UUID.";
UpdateModelParameter::usage = "UpdateModelParameter[id, tag, param, value] updates a parameter of a registered model. \
Cache invalidation is handled automatically by sectoral hashes.";
UpdateModelCache::usage = "UpdateModelCache[id, modelAssoc] safely updates the model in the global registry.";

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


Begin["`Private`"];


$ModelRegistry = <||>; (* Инициализируем реестр заранее *)

(* Рекурсивный обход ассоциации для получения всех путей *)
getAllPaths[assoc_Association, currentPath_List : {}] := 
  Flatten[KeyValueMap[Function[{key, val}, If[AssociationQ[val], getAllPaths[val, Append[currentPath, key]], {Append[currentPath, key]}]], assoc], 1];

(* ════════════════════════════════════════════════════════════════ *)
(* СТРОГИЕ ПРАВИЛА ПОДСТАНОВКИ (НА ЛЕТУ)                       *)
(* ════════════════════════════════════════════════════════════════ *)

GetParameterRules[modelAssoc_Association] := Module[
  {primaryData, valuePaths, rules},
  primaryData = modelAssoc["Primary"];
  valuePaths = Select[getAllPaths[primaryData], Last[#] === "Value" &];
  
  rules = Map[
    Function[valuePath,
      Module[{symbolPath, symbol, val},
        symbolPath = ReplacePart[valuePath, -1 -> "Symbol"];
        symbol = Extract[primaryData, symbolPath];
        val = Extract[primaryData, valuePath];
        (* Используем -> (Rule) вместо :> (RuleDelayed) *)
        symbol -> (val * If[symbol === QED`$PhiExt, QED`$Phi0Value, 1])
      ]
    ],
    valuePaths
  ];
  SortBy[rules, Function[r, ToString[r[[1]]]]]
];

GetStaticRules[modelAssoc_Association] := Join[
  GetParameterRules[modelAssoc],
  {QED`$Phi0 -> QED`$Phi0Value, QED`$hbar -> QED`$hbarValue, QED`$e -> QED`$eValue}
];

GetParameterSymbols[modelAssoc_Association] := Map[First, GetParameterRules[modelAssoc]];

GetParameterVector[modelAssoc_Association] := Developer`ToPackedArray[N[Map[Last, GetParameterRules[modelAssoc]]], Real];

UpdateModelCache[id_String, m_Association] := ($ModelRegistry[id] = m;);

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
          model, circuitImage, id},
    
    method = OptionValue[Method];
    gNode = If[OptionValue[GroundNode] === Automatic, 
       Max[Flatten[components[[All, {2, 3}]]]], OptionValue[GroundNode]];
    
    topology = CreateTopology[components, gNode];    
    defaultPrimary = GenerateDefaultParameters[topology];
    analytical = ComputeAnalyticalParams[topology, defaultPrimary, method];
    
    circuitImage = If[OptionValue[CustomImage] === Automatic,
      GenerateCircuitImage[topology], OptionValue[CustomImage]];
    
    id = CreateUUID["model-"];

    model = <|
      "ModelID" -> id,
      "ModelVersion" -> "1.1",
      "Topology" -> topology,
      "Primary" -> defaultPrimary,
      "Image" -> circuitImage,
      "Analytical" -> analytical,
      "Presets" -> <||>,
      "Numerical" -> <|
        "Method" -> method,
        "VersionedCache" -> <|"Values" -> <||>|>,
        "ComputationTime" -> Null,
        "ComputationStatus" -> <||>
      |>
    |>;

    (* Автоматически регистрируем для работы кэша *)
    $ModelRegistry[id] = model;
    
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
(* ║                  УПРАВЛЕНИЕ И КЭШИРОВАНИЕ                      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)


(* ════════════════════════════════════════════════════════════════ *)
(* ФАЗА 2: ДВИЖОК ЗАВИСИМОСТЕЙ (DEPENDENCY ENGINE)                  *)
(* ════════════════════════════════════════════════════════════════ *)

GetSectoralHashes[modelAssoc_] := Module[
  {primaryData, valuePaths, extractSector},
  
  primaryData = modelAssoc["Primary"];
  
  (* 1. Собираем все пути к значениям (используем уже существующую функцию) *)
  valuePaths = Select[
    QED`Model`Private`getAllPaths[primaryData], 
    Last[#] === "Value" &
  ];
  
  (* 2. Вспомогательная функция сборки хеша сектора *)
  extractSector[paramNamesList_List] := Module[
    {sectorPaths, sectorValues},
    
    (* Фильтруем пути: предпоследний элемент пути - это имя параметра (C, EJ и т.д.) *)
    sectorPaths = Select[valuePaths, MemberQ[paramNamesList, #[[ -2 ]]] &];
    
    (* Жесткая лексикографическая сортировка путей для защиты от смены порядка *)
    sectorPaths = Sort[sectorPaths];
    
    (* Извлекаем сами голые числа по отсортированным путям *)
    sectorValues = Map[Extract[primaryData, #] &, sectorPaths];
    
    Hash[sectorValues]
  ];
  
  (* 3. Распределяем физические параметры по доменам *)
  <|
    "Kinetic" -> extractSector[{"C", "CJ"}],
    "Potential" -> extractSector[{"L", "EJ"}],
    "External" -> extractSector[{"Fext"}]
  |>
];

$DependencyRegistry = <|
  
  "CompiledEngines" -> <|
    "Dependencies" -> {},
    "RelevantHashes" -> {},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{nodes, fluxSymbols, paramSymbols},
        nodes = Cases[modelAssoc["Topology"]["Nodes"], Except[modelAssoc["Topology"]["GroundNode"]]];
        fluxSymbols = Subscript[QED`$FluxSymbol, #] & /@ nodes;
        paramSymbols = GetParameterSymbols[modelAssoc];
        
        QED`Numeric`Calculators`CalcCompiledEngines[
          modelAssoc["Analytical"],
          fluxSymbols,
          paramSymbols
        ]
      ]
    ]
  |>,
  
  "StaticMatrices" -> <|
    "Dependencies" -> {},
    "RelevantHashes" -> {"Kinetic"},
    "Compute" -> Function[{modelAssoc, depsData},
      QED`Numeric`Calculators`CalcStaticMatrices[
        modelAssoc["Analytical"], 
        GetStaticRules[modelAssoc]
      ]
    ]
  |>,
  
  "EquilibriumFluxes" -> <|
    "Dependencies" -> {"CompiledEngines"},
    "RelevantHashes" -> {"Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{engines, paramVector, lastCacheEntry, initialGuess, numVars},
        engines = depsData["CompiledEngines"];
        paramVector = GetParameterVector[modelAssoc];
        
        (* Надежный расчет размерности через топологию *)
        numVars = Length[Cases[modelAssoc["Topology"]["Nodes"], Except[modelAssoc["Topology"]["GroundNode"]]]];
        
        lastCacheEntry = Lookup[modelAssoc["Numerical", "VersionedCache", "Values"], "EquilibriumFluxes", <||>];
        initialGuess = Lookup[lastCacheEntry, "Value", ConstantArray[0., numVars]];
        
        (* УБРАН лишний аргумент extFluxRule *)
        QED`Numeric`Calculators`CalcEquilibrium[engines, initialGuess, paramVector]
      ]
    ]
  |>,
  
  "SystemMatrices" -> <|
    "Dependencies" -> {"CompiledEngines", "EquilibriumFluxes"},
    "RelevantHashes" -> {"Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      QED`Numeric`Calculators`CalcSystemMatrices[
        depsData["CompiledEngines"][[3]], (* fastLInv *)
        depsData["EquilibriumFluxes"],
        GetParameterVector[modelAssoc]
      ]
    ]
  |>,
  
  "HarmonicDiagonalization" -> <|
    "Dependencies" -> {"StaticMatrices", "SystemMatrices"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      QED`Numeric`Calculators`CalcHarmonicDiagonalization[
        depsData["StaticMatrices"][[2]], (* invCNum *)
        depsData["SystemMatrices"]["InverseInductance"]
      ]
    ]
  |>,

  "PlasmonFrequencies" -> <|
    "Dependencies" -> {"HarmonicDiagonalization"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      depsData["HarmonicDiagonalization"]["NormalModeFrequencies"]
    ]
  |>,

  "SMatrix" -> <|
    "Dependencies" -> {"StaticMatrices", "SystemMatrices"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{cNum, invLNum, omega, portIndices, z0, analytical},
        cNum = depsData["StaticMatrices"][[1]]; (* Первая матрица - C *)
        invLNum = depsData["SystemMatrices"]["InverseInductance"];
        analytical = modelAssoc["Analytical"];
        
        (* Извлекаем частоту и порты из правил (на будущее можно вынести в параметры GUI) *)
        omega = ReplaceAll[analytical["Scattering"]["FrequencyVariable"], GetStaticRules[modelAssoc]];
        
        (* Если omega не задана числом, возвращаем Failed *)
        If[!NumericQ[omega], Return[$Failed]];
        
        portIndices = analytical["Scattering"]["PortIndices"];
        z0 = 50.0; (* Базовый импеданс линии *)
        
        QED`Numeric`Calculators`CalcSMatrixNumeric[
          omega, cNum, invLNum, portIndices, z0
        ]
      ]
    ]
  |>,

  "CurrentOperatorNumerical" -> <|
    "Dependencies" -> {"EquilibriumFluxes"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{nodes, minSymbols, phiMinRules, strictRules, cleanRules},
        nodes = Cases[modelAssoc["Topology"]["Nodes"], Except[modelAssoc["Topology"]["GroundNode"]]];
        
        minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
        phiMinRules = Thread[minSymbols -> depsData["EquilibriumFluxes"]];
        
        strictRules = GetStaticRules[modelAssoc];
        Lookup[modelAssoc["Analytical"], "CurrentOperator", 0] /. strictRules /. phiMinRules
      ]
    ]
  |>,

  "VoltageOperatorsNumerical" -> <|
    "Dependencies" -> {"EquilibriumFluxes"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{nodes, minSymbols, phiMinRules, strictRules, cleanRules},
        nodes = Cases[modelAssoc["Topology"]["Nodes"], Except[modelAssoc["Topology"]["GroundNode"]]];
        
        minSymbols = Subscript[QED`$FluxSymbol, "min", #] & /@ nodes;
        phiMinRules = Thread[minSymbols -> depsData["EquilibriumFluxes"]];
        
        strictRules = GetStaticRules[modelAssoc];
        Lookup[modelAssoc["Analytical"], "VoltageOperators", 0] /. strictRules /. phiMinRules
      ]
    ]
  |>
|>;


(* === GLOBAL MODEL REGISTRY === *)
$ModelRegistry = <||>;

RegisterModel[model_Association] := Module[{id, m},
  If[KeyExistsQ[model, "ModelID"],
    id = model["ModelID"];
    $ModelRegistry[id] = model;
    Return[id];
  ];
  id = CreateUUID["model-"];
  m = model;
  m["ModelID"] = id;
  $ModelRegistry[id] = m;
  id
];

GetModel[id_String] := Lookup[$ModelRegistry, id, $Failed];

UpdateModelParameter[id_String, tag_String, param_String, val_] := Module[{m},
  m = GetModel[id];
  If[AssociationQ[m],
    m["Primary", tag, param, "Value"] = val;
    $ModelRegistry[id] = m;
  ];
];

GetNumericalQuantity[modelAssoc_, keyString_String] := Module[
  {registryNode, currentHashes, cachedValues, cachedEntry, isCacheValid, depsData, computedValue, id},
  
  registryNode = Lookup[$DependencyRegistry, keyString, $Failed];
  If[registryNode === $Failed, 
    Message[GetNumericalQuantity::unknown, keyString]; Return[$Failed]
  ];
  
  currentHashes = GetSectoralHashes[modelAssoc];
  cachedValues = Lookup[modelAssoc["Numerical", "VersionedCache"], "Values", <||>];
  cachedEntry = Lookup[cachedValues, keyString, <||>];
  
  isCacheValid = If[Length[cachedEntry] > 0,
    AllTrue[registryNode["RelevantHashes"], currentHashes[#] === cachedEntry["Hashes", #] &],
    False
  ];
  
  If[isCacheValid, Return[cachedEntry["Value"]]];
  
  depsData = AssociationMap[Function[depKey, GetNumericalQuantity[modelAssoc, depKey]], registryNode["Dependencies"]];
  computedValue = registryNode["Compute"][modelAssoc, depsData];
  
  (* Сохраняем результат в Глобальный Реестр вместо $CurrentModel *)
  id = Lookup[modelAssoc, "ModelID", ""];
  If[id != "" && KeyExistsQ[$ModelRegistry, id],
    $ModelRegistry[id, "Numerical", "VersionedCache", "Values", keyString] = <|
      "Value" -> computedValue,
      "Hashes" -> KeyTake[currentHashes, registryNode["RelevantHashes"]]
    |>
  ];
  
  computedValue
];

GetNumericalQuantity::unknown = "Unknown dependency key: `1`";


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


End[];
EndPackage[];