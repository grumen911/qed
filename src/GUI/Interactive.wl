BeginPackage["QED`Interactive`", {"QED`Model`"}];

QubitDashboard::usage = "QubitDashboard[{models..}] - интерактивная панель управления для списка моделей.";
RegisterPlot::usage = "RegisterPlot[id, label, type, computeFunc] регистрирует новый тип графика.";

Begin["`Private`"];

(* ═══════════════════════════════════════════════════════════════ *)
(* 1. BACKEND: PLOT REGISTRY & COMPUTE SYSTEM *)
(* ═══════════════════════════════════════════════════════════════ *)

$PlotRegistry = <||>;

RegisterPlot[id_String, label_String, type_String, computeFunc_] := 
  ($PlotRegistry[id] = <|
    "Label" -> label, 
    "Type" -> type,
    "Compute" -> computeFunc
  |>);

(* Регистрация базовых графиков *)
RegisterPlot["PlasmonSpectrum", "Plasmon Spectrum", "Light", 
  Function[{m}, QED`Plots`PlotPlasmonSpectrum[m]]
];

RegisterPlot["Potential3D", "Potential Landscape 3D", "Heavy", 
  Function[{m}, QED`Plots`PlotPotentialSlices3D[m]]
];

(* DEBUG PLOT: Инспектор кэша (Read-only) *)
RegisterPlot["DebugCache", "Debug Cache Inspector", "Light",
  Function[{m},
    Module[{cache, eqPoints, freqs, isDirty},
      cache = m["Numerical", "Cache"];
      isDirty = m["Numerical", "IsDirty"];
      eqPoints = Lookup[cache, "EquilibriumPoints", "Missing"];
      freqs = Lookup[cache, "PlasmonFrequencies", "Missing"];
      
      Column[{
        Style["Numerical Cache Inspector", Bold, 16], 
        Spacer[10],
        
        Style["Model Status:", Bold],
        Row[{"IsDirty: ", If[TrueQ[isDirty], Style["True", Red], Style["False", Green]]}],
        Spacer[10],
        
        Style["Cache Keys:", Bold],
        If[AssociationQ[cache], Keys[cache], "Not an Association"],
        Spacer[10],
        
        Style["EquilibriumPoints Entry:", Bold],
        If[AssociationQ[eqPoints], 
           Column[{
             "State: " <> ToString[eqPoints["State"]],
             "Solutions Count: " <> If[KeyExistsQ[eqPoints, "Value"], 
                 ToString[Length[eqPoints["Value"]["Solutions"]]], 
                 "No Value"
             ]
           }], 
           eqPoints
        ],
        Spacer[10],
        
        Style["PlasmonFrequencies Entry:", Bold],
        If[AssociationQ[freqs], 
           Column[{
             "State: " <> ToString[freqs["State"]],
             "Value: " <> ToString[Short[freqs["Value"]]]
           }], 
           freqs
        ],
        
        Spacer[20],
        Style["Raw Cache Dump:", Bold],
        Pane[Short[cache, 20], {400, 300}, Scrollbars -> True]
      }]
    ]
  ]
];


(* 
   COMPUTE WORKER (FUNCTIONAL STYLE)
   Input: plotId, model (Value)
   Output: {Graphics, UpdatedModel (Value)}
*)
(* Note: No HoldFirst. Passing by value is safer for functional update. *)
ComputePlotData[plotId_, model_Association] := 
  Block[{$CurrentModel = model},
    
    (* 0. Zombie Protection *)
    If[Length[$CurrentModel["Numerical", "Cache"]] === 0,
       $CurrentModel["Numerical", "IsDirty"] = True;
    ];
    
    (* 1. Warm up Cache (Updates $CurrentModel internally) *)
    QED`Model`GetNumericalQuantity[$CurrentModel, "PlasmonFrequencies"];
    
    If[plotId === "Potential3D",
       QED`Model`GetNumericalQuantity[$CurrentModel, "EquilibriumPoints"]
    ];
    
    (* 2. Compute Graphic using warmed model *)
    Module[{info, func, graphic},
      info = $PlotRegistry[plotId];
      
      graphic = If[MissingQ[info], 
         Graphics[{Red, Text["Unknown Plot ID"]}],
         func = info["Compute"];
         Check[func[$CurrentModel], Graphics[{Red, Text["Computation Failed"]}]]
      ];
      
      (* 3. Return Result AND The Updated Model State *)
      {graphic, $CurrentModel}
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 2. VIEW COMPONENTS: SLIDERS & CONTROLS *)
(* ═══════════════════════════════════════════════════════════════ *)

ExtractInteractiveParams[model_Association] :=
  Flatten[
    KeyValueMap[
      Function[{tag, componentParams},
        KeyValueMap[
          {tag, #1, #2["Value"], {#2["Min"], #2["Max"], #2["Step"]}} &,
          Select[componentParams, AssociationQ[#] && 
            Lookup[#, "Interactive", False] === True &]
        ]
      ],
      model["Primary"]
    ],
    1
  ];

(* MakeParameterControl needs HoldFirst to update the local symbol from UI *)
SetAttributes[MakeParameterControl, HoldFirst];
MakeParameterControl[model_, {tag_, param_, val_, {min_, max_, step_}}, onUpdate_] := 
  Module[{currentVal = val},
    Row[{
      Style[param <> ": ", 12],
      
      Slider[
        Dynamic[
          model["Primary", tag, param, "Value"], 
          
          Function[{v},
            model["Primary", tag, param, "Value"] = v;
            model["Numerical", "IsDirty"] = True;
            onUpdate[]
          ]
        ],
        {min, max, step},
        ImageSize -> 120
      ],
      
      Spacer[5],
      
      InputField[
        Dynamic[
          model["Primary", tag, param, "Value"],
          Function[{v},
            model["Primary", tag, param, "Value"] = v;
            model["Numerical", "IsDirty"] = True;
            onUpdate[]
          ]
        ],
        Number, 
        FieldSize -> {5, 1}
      ]
    }]
  ];

SetAttributes[PlotControlPanel, HoldFirst];
PlotControlPanel[model_, onUpdate_, onForceUpdate_] := 
  Module[{params},
    params = ExtractInteractiveParams[model];
    
    Column[
      Join[
        Map[
          MakeParameterControl[model, #, onUpdate] &,
          params
        ],
        
        {Spacer[10],
         Button["Update Plot", 
           onForceUpdate[],
           Method -> "Queued",
           ImageSize -> {140, 30}
         ]}
      ]
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 3. CORE: QUBIT DASHBOARD *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[modelsStack : {__Association}] := DynamicModule[
  {
    currentModel = First[modelsStack],
    selectedPlotId = "PlasmonSpectrum",
    needsUpdate = False, 
    plotCache
  },
  
  Column[{
    Row[{
      Panel[
        Column[{
          Style["Model: " <> ToString[currentModel["Topology"]["Name"]], Bold],
          Spacer[10],
          
          PlotControlPanel[
            currentModel, 
            Function[{}, 
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                  needsUpdate = True,
                  plotCache[selectedPlotId] = Missing["Stale"]
               ]
            ],
            Function[{}, needsUpdate = True]
          ]
        }],
        Alignment -> Top
      ],
      
      Spacer[20],
      
      Column[{
        Row[{
           "Plot Type: ",
           PopupMenu[Dynamic[selectedPlotId], Keys[$PlotRegistry]]
        }],
        Spacer[10],
        
        Dynamic[
          (* 1. COMPUTE AND UPDATE STATE *)
          If[needsUpdate,
             Module[{res, updatedModel},
               (* Functional Update: Get Result + New State *)
               {res, updatedModel} = ComputePlotData[selectedPlotId, currentModel];
               
               plotCache[selectedPlotId] = res;
               currentModel = updatedModel; (* Explicitly update Dynamic variable *)
               needsUpdate = False;
             ]
          ];
          
          (* 2. RENDER *)
          Module[{cached},
            cached = plotCache[selectedPlotId];
            
            Switch[cached,
              _Missing, 
              If[cached === Missing["Stale"],
                 Panel[Style["Parameters changed. Press Update.", Gray], ImageSize->{300,300}],
                 If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                    needsUpdate = True; "Computing...", 
                    Panel[Style["Select plot to start", Gray], ImageSize->{300,300}]
                 ]
              ],
              
              _, cached
            ]
          ],
          
          TrackedSymbols :> {needsUpdate, selectedPlotId, plotCache}
        ]
      }, Alignment -> Top]
    }, Alignment -> Top],
    
    Dynamic @ Row[{"Cache: ", Keys[plotCache], " | Update: ", needsUpdate}]
  }],
  
  UnsavedVariables :> {plotCache, needsUpdate},
  Initialization :> {
    plotCache = <||>;
    needsUpdate = True; 
    
    (* Force Dirty on Init *)
    currentModel["Numerical", "IsDirty"] = True;
  },
  SynchronousInitialization -> False,
  SaveDefinitions -> False
];

End[];
EndPackage[];