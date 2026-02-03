BeginPackage["QED`Interactive`", {"QED`Model`", "QED`Numeric`"}];

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

(* NEW: Schrödinger Equation Verification Tool *)
RegisterPlot["WaveFunctionCheck", "Verify Harmonic Wavefunctions", "Heavy",
  Function[{m},
    Module[{states, report, grid, nDOF},
      nDOF = m["Topology"]["DegreesOfFreedom"];
      
      states = {{0,0,0}, {1,0,0}, {0,1,0}, {0,0,1}}; (* Default states to check *)
      (* Adjust for actual DOF *)
      states = Select[states, Length[#] == nDOF &];
      If[states === {}, states = {ConstantArray[0, nDOF]}];
      
      report = Map[
        Function[s, 
          QED`Numeric`VerifyWaveFunction[m, s]
        ],
        states
      ];
      
      (* Render Report Table *)
      Grid[
        Prepend[
          Map[
            Function[r, {
              r["State"],
              If[r["Status"] === "OK", Style["OK", Green, Bold], Style["FAIL", Red, Bold]],
              Pane[ScientificForm[r["TotalEnergy"], 5], 100],
              Pane[r["EPsiScale"], 100],
              Pane[r["H_psi"], {250, 120}, Scrollbars -> True], (* H\[Psi] Column *)
              Pane[r["E_psi"], {250, 120}, Scrollbars -> True]  (* E\[Psi] Column *)
            }],
            report
          ],
          {
            Style["State", Bold],
            Style["Status", Bold],
            Style["Total Energy (J)", Bold],
            Style["Scale E\[Psi]", Bold],
            Style["H\[Psi]", Bold],
            Style["E\[Psi]", Bold]
          }
        ],
        Frame -> All,
        Background -> {None, {Lighter[Gray, 0.8], None}},
        ItemSize -> {Automatic, 2.5},
        Alignment -> {Left, Center}
      ]
    ]
  ]
];

(* NEW: Harmonic diagonalization consistency check (Light) *)
RegisterPlot["DiagonalizationCheck", "Verify Harmonic Diagonalization", "Light",
  Function[{m},
    Module[{r, okStyle, failStyle, boolStyle},
      okStyle = Style["OK", Darker[Green, 0.2], Bold];
      failStyle = Style["FAIL", Red, Bold];
      boolStyle = Function[b, If[TrueQ[b], okStyle, failStyle]];

      r = QED`Numeric`VerifyDiagonalization[m];

      If[r === $Failed || FailureQ[r],
        Return[Panel[Style["VerifyDiagonalization failed.", Red], ImageSize -> {600, 200}]]
      ];

      Grid[
        {
          {Style["Check", Bold], Style["Result", Bold]},
          {"Is L transformed diagonal?", boolStyle[r["Is_L_Diagonal"]]},
          {"Is C transformed diagonal?", boolStyle[r["Is_C_Diagonal"]]},
          {"Ceff / Diagonal[N^T C N]", Pane[Short[r["EffectiveCapacitances_Check"], 3], {420, 40}, Scrollbars -> True]},
          {"Transformed C = N^T C N", Pane[MatrixForm[r["Transformed_C"]], {420, 120}, Scrollbars -> True]},
          {"Transformed L = N^T L^-1 N", Pane[MatrixForm[r["Transformed_L_Inverse"]], {420, 120}, Scrollbars -> True]}
        },
        Frame -> All,
        Background -> {None, {Lighter[Gray, 0.8], None}},
        Alignment -> {Left, Center},
        ItemSize -> {Automatic, Automatic}
      ]
    ]
  ]
];

(* NEW: Symbolic WaveFunction Inspector *)
RegisterPlot["SymbolicWaveFunction", "Inspect Symbolic Wave Function", "Light",
  Function[{m},
    Module[{state, psiFormula, nDOF},
      (* Default to ground state *)
      nDOF = m["Topology"]["DegreesOfFreedom"];
      state = ConstantArray[0, nDOF];
      
      (* Get the wavefunction expression *)
      psiFormula = QED`Model`GetWaveFunction[m, state];
      
      If[FailureQ[psiFormula], 
        Return["Failed to generate wavefunction."]
      ];

      (* Keep expression on one line: horizontal scrolling instead of wrapping *)
      Pane[
        psiFormula,
        ImageSize -> {700, 300},
        Scrollbars -> True,
        BaseStyle -> {LineBreakWithin -> False}
      ]
    ]
  ]
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

    (* Explicit warmup for DiagonalizationCheck using GetNumericalQuantity *)
    If[plotId === "DiagonalizationCheck",
       QED`Model`GetNumericalQuantity[$CurrentModel, "HarmonicDiagonalization"]
    ];
    
    (* 2. Compute Graphic using warmed model *)
    Module[{info, func, graphic},
      info = $PlotRegistry[plotId];
      
      graphic = If[MissingQ[info], 
         Graphics[{Red, Text["Unknown Plot ID"]}],
         func = info["Compute"];
         func[$CurrentModel]
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

(* Отрисовка слайдера с именем Tag.Param *)
SetAttributes[MakeParameterControl, HoldFirst];
MakeParameterControl[model_, {tag_, param_, val_, {min_, max_, step_}}, onUpdate_] := 
  Module[{currentVal = val},
    Row[{
      Style[tag <> "." <> param <> ": ", 12],
      
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
        FieldSize -> {6, 1}
      ]
    }]
  ];

(* Выбор модели с картинками (Safe Version) *)
SetAttributes[SelectModel, HoldFirst];
SelectModel[modelSymbol_, modelsStack_List, onUpdate_] :=
  Row[{
    Pane[
      SetterBar[
        Dynamic[modelSymbol, 
           Function[{newModel},
             modelSymbol = newModel;
             onUpdate[]; (* Callback to clear cache/reset UI *)
           ]
        ],
        (* Value (Model) -> Label (Thumbnail Image) *)
        (* Added Lookup for Name safety *)
        (# -> Tooltip[
                 Show[#["Image"], ImageSize->{60,60}, AspectRatio->1, Axes->False, Frame->True, FrameTicks->None], 
                 Lookup[#["Topology"], "Name", "Circuit"]
              ]) & /@ modelsStack,
        Appearance -> "Vertical"
      ],
      ImageSize -> {80, 200},
      Scrollbars -> {False, True}
    ],
    Spacer[10],
    
    (* Big Preview of Current Model *)
    Dynamic[
      Column[{
        Style[Lookup[modelSymbol["Topology"], "Name", "Circuit"], Bold, 12],
        Show[modelSymbol["Image"], ImageSize -> {180, 180}, AspectRatio->1]
      }, Alignment -> Center]
    ]
  }];

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
      (* LEFT PANEL: Model Selection + Controls *)
      Panel[
        Column[{
          (* Model Selector Widget *)
          SelectModel[currentModel, modelsStack, 
             Function[{}, 
               (* Reset state on model switch *)
               plotCache = <||>;
               needsUpdate = True; 
               (* Force dirty to ensure recompute *)
               currentModel["Numerical", "IsDirty"] = True;
             ]
          ],
          
          Spacer[15],
          (* Divider[] REMOVED as requested *)
          Spacer[10],
          
          (* Sliders *)
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
      
      (* RIGHT PANEL: Plot Area *)
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
               {res, updatedModel} = ComputePlotData[selectedPlotId, currentModel];
               plotCache[selectedPlotId] = res;
               currentModel = updatedModel;
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
    currentModel["Numerical", "IsDirty"] = True;
  },
  SynchronousInitialization -> False,
  SaveDefinitions -> False
];

End[];
EndPackage[];
