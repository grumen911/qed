BeginPackage["QED`Model`"];

Needs["QED`CircuitTopology`"];
Needs["QED`Numeric`"];
Needs["QED`Analytic`"];
Needs["QED`Scattering`"];

CreateCircuitModel::usage = "CreateCircuitModel[components, Options] initializes a new \
superconducting circuit model from a list of components.

Arguments:
  components: A list of circuit elements defining the graph, e.g., 
              {{\"Capacitor\", 1, 2, \"C1\"}, {\"JosephsonJunction\", 2, 3, \"EJ1\"}, ...}.

Options:
  GroundNode  -> Automatic (Defaults to the highest node index in the list)
  CustomImage -> Automatic (Generates a default placeholder diagram)

Under the hood, this function performs the complete setup pipeline:
  1. Parses the netlist to build the graph topology.
  2. Generates default physical parameters (Primary) with dynamic UI bounds.
  3. Computes symbolic Lagrangians, Hamiltonians, and static matrices (Analytical).
  4. Assigns a unique UUID and safely registers the model in the global $ModelRegistry.
  5. Triggers a cache warm-up for JIT-compiled engines to ensure instant UI responsiveness.

Returns the fully initialized model Association.";


GetNumericalQuantity::usage = "GetNumericalQuantity[model, \"key\"] retrieves \
a computed numerical property from the model.

If the requested \"key\" is already calculated and valid, it returns the value instantly (O(1)).
If the value is marked as \"Lazy\" or the cache was invalidated (e.g., due to parameter updates), \
this function automatically triggers the necessary background computations, atomically updates \
the global $ModelRegistry, and returns the fresh result.

Common keys include:
  \"EquilibriumFluxes\"       - Minimum potential energy points (Weber)
  \"StaticMatrices\"          - Fixed capacitance and inductance matrices
  \"CompiledEngines\"         - JIT-compiled Hessian and gradient functions
  \"HarmonicDiagonalization\" - Frequencies and transformation matrices (N, M)

This function acts as the primary lazy-evaluation bridge between the UI and the mathematical kernel.";


GetParameterRules::usage = "GetParameterRules[model] dynamically generates a sorted list of \
strict substitution rules (Symbol -> Value) for all primary physical components (e.g., C, EJ, L) \
based on their current UI values.

Crucially, this function performs automatic unit scaling for the external magnetic flux: \
the dimensionless slider value for Φ_ext is automatically multiplied by the magnetic flux \
quantum (Φ_0) to return the parameter in absolute SI units (Webers).

These rules are primarily used to inject real-time parameter states into analytical \
Hamiltonians and static matrices.";


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

GetParameterVector::usage = "GetParameterVector[model] returns a sorted PackedArray of Reals representing \
the model's primary parameters for JIT compilation.";


Begin["`Private`"];


Options[CreateCircuitModel] = {
  GroundNode -> Automatic,
  CustomImage -> Automatic
};

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                      1. СОСТОЯНИЕ (STATE)                      ║ *)
(* ║     (Глобальный реестр моделей, защищенный от перезаписи)      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

$ModelRegistry = <||>;

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                   2. ВНУТРЕННИЕ УТИЛИТЫ                        ║ *)
(* ║   (Вспомогательные функции, парсеры и генераторы правил)       ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

(* Рекурсивный обход ассоциации для получения всех путей *)
getAllPaths[assoc_Association, currentPath_List : {}] := 
  Flatten[KeyValueMap[Function[{key, val}, If[AssociationQ[val], getAllPaths[val, Append[currentPath, key]], {Append[currentPath, key]}]], assoc], 1];

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
  {QED`$Phi0 -> QED`$Phi0Value, QED`$hbar -> QED`$hbarValue, QED`$e -> QED`$eValue, QED`$Z0 -> QED`$Z0Value}
];

GetParameterSymbols[modelAssoc_Association] := Map[First, GetParameterRules[modelAssoc]];

GetParameterVector[modelAssoc_Association] := Developer`ToPackedArray[N[Map[Last, GetParameterRules[modelAssoc]]], Real];

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
(* ║                    3. ФАБРИКА МОДЕЛЕЙ                          ║ *)
(* ║       (Инициализация графа, параметров и гамильтониана)        ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)
  
CreateCircuitModel[components_List, opts : OptionsPattern[]] := 
  Module[{analytical, defaultPrimary, topology, gNode, 
          model, circuitImage, id},
    
    gNode = If[OptionValue[GroundNode] === Automatic, 
       Max[Flatten[components[[All, {2, 3}]]]], OptionValue[GroundNode]];
    
    topology = CreateTopology[components, gNode];    
    defaultPrimary = GenerateDefaultParameters[topology];
    analytical = ComputeAnalyticalParams[topology, defaultPrimary];
    
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
        "VersionedCache" -> <|"Values" -> <||>|>,
        "ComputationTime" -> Null,
        "ComputationStatus" -> <||>
      |>
    |>;

    (* Автоматически регистрируем для работы кэша *)
    $ModelRegistry[id] = model;
    GetNumericalQuantity[model, "CompiledEngines"];

    model = $ModelRegistry[id];
    
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

ComputeAnalyticalParams[topology_, primaryParams_] := 
 Module[{lagrangian, capMatrix, indMatrix, hamiltonian, harmonicHamiltonian,
 		 potentialGradient, currentOp, voltageOperatorsSym, nodes, 
     scattering12, scattering14, scattering, bicCondition12, bicCondition14},
  
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

(* Вычисляем S-матрицу для портов {1, 2} (считаем, что они всегда есть) *)
  scattering12 = QED`Scattering`BuildSymbolicScattering[topology, primaryParams, Ports -> {1, 2}, ReferenceImpedance -> QED`$Z0];
  
  (* Безопасное вычисление S-матрицы для портов {1, 4} *)
  scattering14 = If[MemberQ[topology["Nodes"], 4],
      QED`Scattering`BuildSymbolicScattering[topology, primaryParams, Ports -> {1, 4}, ReferenceImpedance -> QED`$Z0],
      $Failed
  ];

  (* Вычисляем гибридное условие BIC, передавая правило зануления CJ для аналитики *)
  bicCondition12 = If[scattering12 =!= $Failed,
      QED`Scattering`BuildSymbolicBICCondition[scattering12, 
          SimplificationRules -> {Subscript[QED`$JosephsonCapacitanceSymbol, _] -> 0}
      ],
      $Failed
  ];

  (* Вычисляем гибридное условие BIC, передавая правило зануления CJ для аналитики *)
  bicCondition14 = If[scattering14 =!= $Failed,
      QED`Scattering`BuildSymbolicBICCondition[scattering14, 
          SimplificationRules -> {Subscript[QED`$JosephsonCapacitanceSymbol, _] -> 0}
      ],
      $Failed
  ];

  (* Упаковываем в ассоциацию *)
  scattering = <|
      "1_2" -> scattering12, 
      "1_4" -> scattering14
  |>;

  <|
    "CapacitanceMatrix" -> capMatrix,
    "Hamiltonian" -> hamiltonian,
    "HarmonicHamiltonian" -> harmonicHamiltonian,
    "InductanceMatrix" -> indMatrix,
    "PotentialGradient" -> potentialGradient,
    "Potential" -> hamiltonian /. Subscript[QED`$ChargeSymbol, _] -> 0,
    "CurrentOperator" -> currentOp,
    "VoltageOperators" -> voltageOperatorsSym,
    "Scattering" -> scattering,
    "BICCondition_1_2" -> bicCondition12,
    "BICCondition_1_4" -> bicCondition14
  |>
 ];

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║               4. ДВИЖОК ЗАВИСИМОСТЕЙ (CONFIG)                  ║ *)
(* ║   (Граф ленивых вычислений и JIT-компиляции, обновляемый)      ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

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

"SMatrix_1_2" -> <|
    "Dependencies" -> {"StaticMatrices", "SystemMatrices"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{cNum, invLNum, omega, portIndices, z0, analytical, scatData},
        cNum = depsData["StaticMatrices"][[1]];
        invLNum = depsData["SystemMatrices"]["InverseInductance"];
        analytical = modelAssoc["Analytical"];
        
        scatData = analytical["Scattering"]["1_2"];
        If[scatData === $Failed, Return[$Failed]];

        omega = ReplaceAll[scatData["FrequencyVariable"], GetStaticRules[modelAssoc]];
        If[!NumericQ[omega], Return[$Failed]];
        
        portIndices = scatData["PortIndices"];
        z0 = QED`$Z0Value;
        
        QED`Numeric`Calculators`CalcSMatrixNumeric[omega, cNum, invLNum, portIndices, z0]
      ]
    ]
  |>,

  "SMatrix_1_4" -> <|
    "Dependencies" -> {"StaticMatrices", "SystemMatrices"},
    "RelevantHashes" -> {"Kinetic", "Potential", "External"},
    "Compute" -> Function[{modelAssoc, depsData},
      Module[{cNum, invLNum, omega, portIndices, z0, analytical, scatData},
        cNum = depsData["StaticMatrices"][[1]];
        invLNum = depsData["SystemMatrices"]["InverseInductance"];
        analytical = modelAssoc["Analytical"];
        
        scatData = analytical["Scattering"]["1_4"];
        If[scatData === $Failed, Return[$Failed]];

        omega = ReplaceAll[scatData["FrequencyVariable"], GetStaticRules[modelAssoc]];
        If[!NumericQ[omega], Return[$Failed]];
        
        portIndices = scatData["PortIndices"];
        z0 = QED`$Z0Value;
        
        QED`Numeric`Calculators`CalcSMatrixNumeric[omega, cNum, invLNum, portIndices, z0]
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

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                    5. ПУБЛИЧНОЕ API                            ║ *)
(* ║    (Handle-Based управление моделями и доступ к кэшу)          ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

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

UpdateModelCache[id_String, m_Association] := ($ModelRegistry[id] = m;);

UpdateModelParameter[id_String, tag_String, param_String, val_] := Module[{m},
  m = GetModel[id];
  If[AssociationQ[m],
    m["Primary", tag, param, "Value"] = val;
    $ModelRegistry[id] = m;
  ];
];

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                 6. ДОПОЛНИТЕЛЬНЫЕ МОДУЛИ                       ║ *)
(* ║           (Волновые функции и система пресетов)                ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

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

SavePreset[id_String, name_String] := Module[{m},
  m = GetModel[id];
  If[!AssociationQ[m] || name === "", Return[$Failed]];
  
  m["Presets", name] = m["Primary"];
  $ModelRegistry[id] = m;
];

LoadPreset[id_String, name_String] := Module[{m},
  m = GetModel[id];
  If[!AssociationQ[m] || !KeyExistsQ[m["Presets"], name], Return[$Failed]];
  
  m["Primary"] = m["Presets", name];
  $ModelRegistry[id] = m;
];

MergePresets[id_String, newPresets_Association] := Module[{m},
  m = GetModel[id];
  If[!AssociationQ[m], Return[$Failed]];
  
  m["Presets"] = Join[m["Presets"], newPresets];
  $ModelRegistry[id] = m;
];

DeletePreset[id_String, name_String] := Module[{m},
  m = GetModel[id];
  If[!AssociationQ[m], Return[$Failed]];
  
  m["Presets"] = KeyDrop[m["Presets"], name];
  $ModelRegistry[id] = m;
];

GetPresetNames[id_String] := Module[{m},
  m = GetModel[id];
  If[!AssociationQ[m], Return[{}]];
  Keys[m["Presets"]]
];


End[];
EndPackage[];