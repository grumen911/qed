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
  (* ════════════════════════════════════════════════════════════ *)
  (* LIGHT PLOTS (fast, auto-update on parameter change)         *)
  (* ════════════════════════════════════════════════════════════ *)
  
  "PlasmonSpectrum" -> <|
    "Label" -> "Plasmon Spectrum",
    "Type" -> "Light",
    "Compute" -> Function[{m}, QED`Plots`PlotPlasmonSpectrum[m]]
  |>,
  
  (* ════════════════════════════════════════════════════════════ *)
  (* HEAVY PLOTS (slow, manual update via button)                *)
  (* ════════════════════════════════════════════════════════════ *)
  
  "Potential3D" -> <|
    "Label" -> "Potential Landscape 3D",
    "Type" -> "Heavy",
    "Compute" -> Function[{m}, QED`Plots`PlotPotentialSlices3D[m]]
  |>
|>;

(* ═══════════════════════════════════════════════════════════════ *)
(* SLIDER WITH CACHE INVALIDATION *)
(* ═══════════════════════════════════════════════════════════════ *)

(* Create dynamic slider with invalidation callback *)
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
        
        (* Trigger cache invalidation if callback provided *)
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

(* Create slider hub with invalidation support *)
MakeSliderHubWithInvalidation[model_, invalidationCallback_] := 
  Module[{params},
    params = ExtractInteractiveParams[model];
    Column[
      Map[
        Function[{paramList},
          MakeDynamicSliderWithInvalidation[
            Unevaluated@model,
            Sequence @@ paramList,
            invalidationCallback[]
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
    
    (* Plot management state *)
    selectedPlot = "PlasmonSpectrum",
    plotCache = <||>,
    needsUpdate = False,
    
    (* Plot registry *)
    plotRegistry = InitPlotRegistry[]
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
      (* ═══════════════════════════════════════════════════════════ *)
      (* TOP ROW: Sliders (left) + Plot (right) *)
      (* ═══════════════════════════════════════════════════════════ *)
      Row[{
        (* LEFT COLUMN: Model selector + Sliders + Update button *)
        Column[{
          SelectModel[Unevaluated@model, modelsStack],
          
          Spacer[10],
          
          (* Sliders with cache invalidation *)
          MakeSliderHubWithInvalidation[
            Unevaluated@model,
            invalidateHeavyPlots  (* Callback to invalidate cache *)
          ],
          
          Spacer[10],
          
          (* Update button (only visible for Heavy plots) *)
          Dynamic[
            If[plotRegistry[selectedPlot]["Type"] === "Heavy",
              Button["Update Plot",
                needsUpdate = True,
                Method -> "Queued",
                ImageSize -> {150, 30}
              ],
              
              (* For Light plots: show info text *)
              Style["(Light plot: auto-updates)", 12, Gray, Italic]
            ],
            TrackedSymbols :> {selectedPlot}
          ]
        }, Alignment -> Top],
        
        Spacer[20],
        
        (* RIGHT COLUMN: Plot selector + Plot display *)
        Column[{
          (* Plot selector dropdown *)
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
                (* ════════════════════════════════════════════════════════ *)
                (* HEAVY PLOT *)
                (* ════════════════════════════════════════════════════════ *)
                plotType === "Heavy",
                  If[needsUpdate,
                    AppendTo[$DebugLog, "needsUpdate=True"];
                    $CurrentModel["Numerical"]["IsDirty"] = True;
                    
                    Module[{computed},
                      computed = plotInfo["Compute"][$CurrentModel];
                      AppendTo[$DebugLog, {"Computed", Head[computed]}];
                      
                      plotCache[selectedPlot] = <|
                        "Plot" -> computed,
                        "Status" -> "UpToDate",
                        "Timestamp" -> Now
                      |>;
                      
                      AppendTo[$DebugLog, {"Cached", Keys[plotCache]}];
                    ];
                    
                    needsUpdate = False;
                  ];
                  
                  (* Отображение *)
                  Module[{display},
                    (* ATOMIC ACCESS FIX: Use With/AssociationQ to prevent check-then-act race conditions *)
                    display = With[{entry = plotCache[selectedPlot]},
                      If[AssociationQ[entry],
                        Module[{cached},
                          cached = entry["Plot"];
                          AppendTo[$DebugLog, {"Retrieved", Head[cached]}];
                          cached
                        ],
                        
                        AppendTo[$DebugLog, "ShowingPlaceholder"];
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
                    
                    AppendTo[$DebugLog, {"DisplayHead", Head[display]}];
                    display
                  ],
                
                plotType === "Light",
                  $CurrentModel["Numerical"]["IsDirty"] = True;
                  plotInfo["Compute"][$CurrentModel],
                
                True,
                  Graphics[
                    Text[Style["Unknown plot type: " <> ToString[plotType], 14, Red]],
                    ImageSize -> 400
                  ]
              ];
              
              AppendTo[$DebugLog, {"WhichResult", Head[result]}];
              result
            ],
            
            TrackedSymbols :> {needsUpdate, selectedPlot, $CurrentModel},
            SynchronousUpdating -> False
          ]

        }, Alignment -> Left]
      }, Alignment -> Top],
      
      Spacer[20],
      
      (* ═══════════════════════════════════════════════════════════ *)
      (* BOTTOM SECTION: Numerical parameters display *)
      (* ═══════════════════════════════════════════════════════════ *)
      Dynamic[
        Column[{
          "PlasmonFrequencies:",
          GetNumericalQuantity[$CurrentModel, "PlasmonFrequencies"],
          "Equilibrium Fluxes:",
          GetNumericalQuantity[$CurrentModel, "EquilibriumFluxes"] /. 
            (a_ -> b_) :> (a -> b/(2.067833848 * 10.^-15)),
          GetNumericalQuantity[$CurrentModel, "HarmonicDiagonalization"],
          $CurrentModel["SubstitutionRules"] // Values
        }],
        TrackedSymbols :> {$CurrentModel}
      ]
    }]
  ]
];

End[];
EndPackage[];