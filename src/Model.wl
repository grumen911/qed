BeginPackage["QED`CircuitMode`"];

CreateCircuitModel::usage = "CreateCircuitModel[topology, primaryParams, method]"
GetAnalyticalParams::usage = "GetAnalyticalParams[model]"
GetNumericalParams::usage = "GetNumericalParams[model]"
UpdatePrimaryParam::usage = "UpdatePrimaryParam[model, path, value]"

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
        "ComputationTime" -> Null
      |>
    |>;
    
    structure
  ]


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║         УРОВЕНЬ 2: ANALYTICAL PARAMETERS                      ║ *)
(* ║  (Гамильтониан, матрицы, представления - символическое)      ║ *)
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

End[];
EndPackage[];