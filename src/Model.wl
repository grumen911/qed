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

GetSymbolic[model_, key_] := model["Symbolic"][key];
GetNumeric[model_, key_]  := model["Numeric"][key];

End[];
EndPackage[];