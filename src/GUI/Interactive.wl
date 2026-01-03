BeginPackage["QED`Interactive`", {"QED`Model`"}];

QubitDashboard::usage = "QubitDashboard[model] - интерактивная панель управления";

Begin["`Private`"];

$DebugLog = {};

(* ═══════════════════════════════════════════════════════════════ *)
(* ЛОГИКА *)
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

(* ═══════════════════════════════════════════════════════════════ *)
(* PLOT REGISTRY *)
(* ═══════════════════════════════════════════════════════════════ *)

(* Initialize plot registry with available plots *)
InitPlotRegistry[] := <|
  "PlasmonSpectrum" -> <|
    "Label" -> "Plasmon Spectrum",
    "Type" -> "Light",
    "Compute" -> Function[{m}, QED`Plots`PlotPlasmonSpectrum[m]]
  |>,
  "Potential3D" -> <|
    "Label" -> "Potential Landscape 3D",
    "Type" -> "Heavy",
    "Compute" -> Function[{m}, QED`Plots`PlotPotentialSlices3D[m]]
  |>
|>;

(* ═══════════════════════════════════════════════════════════════ *)
(* SLIDER WITH CACHE INVALIDATION *)
(* ═══════════════════════════════════════════════════════════════ *)

MakeDynamicSliderWithInvalidation[
  model_, componentTag_, paramName_, currentValue_, 
  {min_, max_, step_}, invalidationCallback_: Null
] := With[{
    d = Dynamic[
      model["Primary"][componentTag][paramName]["Value"], 
      (
        model["Primary"][componentTag][paramName]["Value"] = #;
        model["Numerical"]["IsDirty"] = True;
        $CurrentModel["Primary"][componentTag][paramName]["Value"] = #;
        $CurrentModel["Numerical"]["IsDirty"] = True;
        If[invalidationCallback =!= Null, invalidationCallback[]];
      ) &
    ]
  },
  Row[{
    componentTag <> "." <> paramName <> ": ",
    Slider[d, {min, max, step}],
    InputField[d, Number, FieldSize -> {6, 1}]
  }]
];

MakeSliderHubWithInvalidation[model_, invalidationCallback_] := 
  Module[{params},
    params = ExtractInteractiveParams[model];
    Column[
      Map[
        Function[{paramList},
          MakeDynamicSliderWithInvalidation[
            Unevaluated@model,
            Sequence @@ paramList,
            invalidationCallback
          ]
        ],
        params
      ]
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* MODEL SELECTOR *)
(* ═══════════════════════════════════════════════════════════════ *)

SelectModel[model_, modelsStack_] :=
  Row[{
    Pane[
      SetterBar[
        Dynamic[model, (model = #; $CurrentModel = #) &],
        MapThread[#2 -> #1 &,
          {Query[All, "Image"][modelsStack], modelsStack}
        ],
        Appearance -> "Vertical"
      ],
      ImageSize -> {All, 200},
      Scrollbars -> {False, True}
    ],
    
    Dynamic[
      Graphics[
        model["Image"],
        ImageSize -> {All, 200}
      ],
      TrackedSymbols :> {model}
    ]
  }];

(* ═══════════════════════════════════════════════════════════════ *)
(* MAIN DASHBOARD *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[modelsStack : {Association__}] := DynamicModule[
  {
    model = First@modelsStack,
    selectedPlot = "PlasmonSpectrum",
    needsUpdate = False,
    plotRegistry = InitPlotRegistry[],
    (* Uninitialized variable for cache *)
    plotCache
  },
  
  (* Helper: Invalidate all Heavy plots in cache *)
  Module[{invalidateHeavyPlots},
    
    invalidateHeavyPlots[] := Module[{},
      Do[
        If[plotRegistry[plotID]["Type"] === "Heavy" && 
           KeyExistsQ[plotCache, plotID],
          plotCache[plotID]["Status"] = "Stale"
        ],
        {plotID, Keys[plotRegistry]}
      ];
    ];
    
    (* Main UI *)
    Column[{
      Row[{
        Column[{
          SelectModel[Unevaluated@model, modelsStack],
          Spacer[10],
          MakeSliderHubWithInvalidation[
            Unevaluated@model,
            invalidateHeavyPlots
          ],
          Spacer[10],
          Dynamic[
            If[plotRegistry[selectedPlot]["Type"] === "Heavy",
              Button["Update Plot",
                needsUpdate = True,
                Method -> "Queued",
                ImageSize -> {150, 30}
              ],
              Style["(Light plot: auto-updates)", 12, Gray, Italic]
            ],
            TrackedSymbols :> {selectedPlot}
          ]
        }, Alignment -> Top],
        
        Spacer[20],
        
        Column[{
          Row[{
            Style["Select Plot: ", Bold],
            PopupMenu[
              Dynamic[selectedPlot],
              KeyValueMap[#1 -> #2["Label"] &, plotRegistry],
              ImageSize -> 200
            ]
          }],
          
          Spacer[10],
          
          (* Plot display area *)
          Dynamic[
            Module[{plotInfo, plotType, result},
              plotInfo = plotRegistry[selectedPlot];
              plotType = plotInfo["Type"];
              
              result = Which[
                plotType === "Heavy",
                  If[needsUpdate,
                    AppendTo[$DebugLog, "needsUpdate=True"];
                    $CurrentModel["Numerical"]["IsDirty"] = True;
                    
                    Module[{computed},
                      computed = plotInfo["Compute"][$CurrentModel];
                      
                      (* Write to Cache *)
                      plotCache[selectedPlot] = <|
                        "Plot" -> computed,
                        "Status" -> "UpToDate",
                        "Timestamp" -> Now
                      |>;
                      
                      AppendTo[$DebugLog, {"CachedKey", selectedPlot}];
                    ];
                    
                    needsUpdate = False;
                  ];
                  
                  (* Read from Cache *)
                  Module[{display},
                    (* Clean Atomic Access *)
                    display = With[{entry = plotCache[selectedPlot]},
                      If[AssociationQ[entry],
                        entry["Plot"],
                        
                        (* Placeholder *)
                        Framed[
                          Pane[
                            Style["Click 'Update Plot' to compute", 16, Gray, Bold],
                            ImageSize -> {380, 380},
                            Alignment -> Center
                          ],
                          Background -> GrayLevel[0.97],
                          FrameStyle -> GrayLevel[0.8],
                          ImageSize -> 400
                        ]
                      ]
                    ];
                    display
                  ],
                
                plotType === "Light",
                  $CurrentModel["Numerical"]["IsDirty"] = True;
                  plotInfo["Compute"][$CurrentModel],
                
                True,
                  Graphics[Text[Style["Unknown", Red]]]
              ];
              result
            ],
            
            (* Track cache *)
            TrackedSymbols :> {needsUpdate, selectedPlot, $CurrentModel, plotCache},
            SynchronousUpdating -> False
          ]

        }, Alignment -> Left]
      }, Alignment -> Top],
      
      Spacer[20],
      
      Dynamic[
        Column[{
          "PlasmonFrequencies:",
          GetNumericalQuantity[$CurrentModel, "PlasmonFrequencies"],
          $CurrentModel["SubstitutionRules"] // Values
        }],
        TrackedSymbols :> {$CurrentModel}
      ]
    }]
  ],
  
  (* CRITICAL: Prevent saving cache state in notebook file *)
  UnsavedVariables :> {plotCache},
  
  (* Initialize cache cleanly on every load *)
  Initialization :> {
    plotCache = <||>;
  }
];

End[];
EndPackage[];