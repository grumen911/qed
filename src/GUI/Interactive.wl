BeginPackage["QED`Interactive`", {"QED`Model`"}];

QubitDashboard::usage = "QubitDashboard[model] - интерактивная панель управления";

Begin["`Private`"];

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
    "Compute" -> Function[{m}, GetNumericalQuantity[m, "PlotPotentialSlices3D"]]
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
        If[invalidationCallback =!= Null, invalidationCallback];
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
            Module[{plotInfo, plotType},
              plotInfo = plotRegistry[selectedPlot];
              plotType = plotInfo["Type"];
              
              Which[
                (* ════════════════════════════════════════════════════════ *)
                (* HEAVY PLOT: Manual update via button                    *)
                (* ════════════════════════════════════════════════════════ *)
                plotType === "Heavy",
                  (* Check if update requested *)
                  If[needsUpdate,
                    (* Force Model cache refresh *)
                    $CurrentModel["Numerical"]["IsDirty"] = True;
                    
                    (* Compute plot and cache result *)
                    plotCache[selectedPlot] = <|
                      "Plot" -> plotInfo["Compute"][$CurrentModel],
                      "Status" -> "UpToDate",
                      "Timestamp" -> Now
                    |>;
                    
                    needsUpdate = False;
                  ];
                  
                  (* Display cached plot or placeholder *)
                  If[KeyExistsQ[plotCache, selectedPlot],
                    plotCache[selectedPlot]["Plot"],
                    Graphics[
                      Text[Style["Click 'Update Plot' to compute", 14, Gray]],
                      ImageSize -> 400,
                      PlotRange -> {{0, 1}, {0, 1}}
                    ]
                  ],
                
                (* ════════════════════════════════════════════════════════ *)
                (* LIGHT PLOT: Always recompute on parameter change        *)
                (* ════════════════════════════════════════════════════════ *)
                plotType === "Light",
                  (* Always refresh Model cache and compute *)
                  $CurrentModel["Numerical"]["IsDirty"] = True;
                  plotInfo["Compute"][$CurrentModel],
                
                (* ════════════════════════════════════════════════════════ *)
                (* UNKNOWN TYPE: Error message                             *)
                (* ════════════════════════════════════════════════════════ *)
                True,
                  Graphics[
                    Text[Style["Unknown plot type: " <> ToString[plotType], 14, Red]],
                    ImageSize -> 400
                  ]
              ]
            ],
            
            (* Track all relevant variables *)
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
