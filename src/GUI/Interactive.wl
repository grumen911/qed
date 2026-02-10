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

RegisterPlot["PlasmonSpectrum (Generic)", "Plasmon Spectrum (Generic)", "Light", 
  Function[{m}, QED`Plots`PlotGenericFluxSweep[m]]
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
              Pane[r["Norm"], 100],
              Pane[r["H_psi"], {250, 120}, Scrollbars -> True], (* H\[Psi] Column *)
              Pane[r["E_psi"], {250, 120}, Scrollbars -> True]  (* E\[Psi] Column *)
            }],
            report
          ],
          {
            Style["State", Bold],
            Style["Status", Bold],
            Style["Total Energy (J)", Bold],
            Style["Norm \[Psi]", Bold],
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

(* NEW: Spectroscopy Scanner (Real-time) *)
RegisterPlot["SpectroscopyScanner", "Spectroscopy Scanner", "Light",
  Function[{m},
    Module[{
        truncationDim = 5, (* Оптимизация для UI: 5 уровней на моду *)
        numLevels = 8,     (* Показываем первые 8 собственных чисел *)
        basis, fluxOps, hTotal, 
        evals, evecs, energies, states,
        numModes, nOps, groundEnergy, hbar,
        rows, freqStr, assignStr, nVals, rowStyle,
        diagData
    },
      (* 1. ПОЛУЧЕНИЕ ДАННЫХ МОДЕЛИ *)
      diagData = QED`Model`GetNumericalQuantity[m, "HarmonicDiagonalization"];
      If[MissingQ[diagData] || FailureQ[diagData], 
         Return[Panel[Style["Model analysis failed. Check parameters.", Red], ImageSize -> {300, 50}]]
      ];
      
      numModes = Length[diagData["NormalModeFrequencies"]];
      If[numModes == 0, Return[Panel["No modes found."]]];

      (* 2. ВЫЧИСЛЕНИЕ ГАМИЛЬТОНИАНА *)
      Quiet[
          basis = QED`Numeric`GetBasisOperators[ConstantArray[truncationDim, numModes]];
          fluxOps = QED`Numeric`ConstructFluxOperators[m, basis];
          
          (* Вызываем исправленную функцию с учетом Hlin и SubstitutionRules *)
          hTotal = QED`Numeric`BuildNumericalHamiltonian[m, basis, fluxOps];
      ];

      (* Защита от старых ошибок в Numeric.wl *)
      If[!FreeQ[hTotal, Complex], 
         Return[Panel[Style["Error: Hamiltonian is Complex!", Red, Bold]]]
      ];

      (* 3. ДИАГОНАЛИЗАЦИЯ *)
      {evals, evecs} = Eigensystem[hTotal, -numLevels];
      
      (* Сортировка по возрастанию энергии *)
      With[{ord = Ordering[evals]},
          energies = evals[[ord]];
          states = evecs[[ord]];
      ];

      groundEnergy = energies[[1]];
      (* Операторы числа фотонов для анализа состава состояний *)
      nOps = Table[basis["ad"][[k]] . basis["a"][[k]], {k, numModes}];
      hbar = QED`$hbarValue;

      (* 4. ФОРМАТИРОВАНИЕ ТАБЛИЦЫ *)
      rows = {{
          Style["Idx", Bold], 
          Style["Freq (GHz)", Bold], 
          Sequence @@ Table[Style["<n" <> ToString[k] <> ">", Bold], {k, numModes}],
          Style["State", Bold]
      }};

      Do[
          (* Вычисляем средние числа заполнения <n> для каждой моды *)
          nVals = Table[Re[states[[i]] . nOps[[k]] . states[[i]]], {k, numModes}];
          
          (* Частота перехода 0 -> i в ГГц *)
          freqStr = NumberForm[(energies[[i]] - groundEnergy) / hbar / 2. / Pi / 10^9, {5, 3}];
          
          (* Строковое представление состояния, например |0,1,0> *)
          assignStr = "|" <> StringRiffle[Round[nVals], ","] <> ">";
          
          (* Логика валидации: Если основное состояние (Idx=1) содержит фотоны -> ОШИБКА *)
          rowStyle = If[i == 1 && Total[nVals] > 0.15, Red, Black];

          AppendTo[rows, {
              Style[i, rowStyle],
              Style[freqStr, rowStyle],
              Sequence @@ (Style[NumberForm[#, {3, 2}], rowStyle] & /@ nVals),
              Style[assignStr, rowStyle]
          }];
      , {i, Length[energies]}];

      (* Возврат Grid для отображения в Dashboard *)
      Column[{
         Text[Style["Spectroscopy Scanner", 16, FontFamily -> "Helvetica"]],
         Text[Style["(Real-time update)", Gray, 10]],
         Spacer[5],
         Grid[rows, 
              Frame -> All, 
              Background -> {None, {1 -> LightGray}}, 
              ItemStyle -> {Automatic, Automatic},
              Alignment -> {Center, Center},
              Spacings -> {1.2, 0.8}
         ]
      }, Alignment -> Center]
    ]
  ]
];

RegisterPlot["LabMatrixElements", "Matrix Elements (Lab Basis)", "Light", 
  Function[{m}, QED`Plots`PlotLabMatrixElements[m]]
];

RegisterPlot["FermiRates", "T1 Relaxation Times (Fermi)", "Light", 
  Function[{m}, QED`Plots`PlotFermiRates[m]]
];

RegisterPlot["RelaxationTime", "Relaxation Time (T1)", "Heavy", 
  Function[{m}, QED`Plots`PlotRelaxationTime[m]]
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

SetAttributes[PresetControlPanel, HoldFirst];
PresetControlPanel[modelSymbol_, onModelUpdate_] := 
  DynamicModule[{selectedPreset = Null},
    Framed[  (* <--- БЫЛО FrameBox, СТАЛО Framed *)
      Row[{
        Style["Presets: ", 10, Gray],
        
        (* 1. Preset Selector *)
        PopupMenu[
          Dynamic[selectedPreset],
          QED`Model`GetPresetNames[modelSymbol],
          "Select...",
          ImageSize -> {90, Automatic}
        ],
        Spacer[5],
        
        (* 2. Load Button *)
        Button[
          Tooltip[Style["Load", 10], "Load selected preset"],
          If[StringQ[selectedPreset],
             Module[{updated},
               updated = QED`Model`LoadPreset[modelSymbol, selectedPreset];
               onModelUpdate[updated]; 
             ]
          ],
          Enabled -> Dynamic[StringQ[selectedPreset]],
          ImageSize -> {40, 20}
        ],
        Spacer[2],
        
        (* 3. Save Button *)
        Button[
          Tooltip[Style["Save", 10], "Save current configuration"],
          Module[{name},
             (* Modal Dialog for Name Input *)
             name = DialogInput[{text = ""}, 
                Column[{
                  Style["Save Preset", Bold],
                  InputField[Dynamic[text], String],
                  Row[{
                    DefaultButton["Save", DialogReturn[text]], 
                    CancelButton[]
                  }]
                }]
             ];
             
             (* Logic if name provided *)
             If[StringQ[name] && StringLength[name] > 0,
                Module[{updated},
                   updated = QED`Model`SavePreset[modelSymbol, name];
                   onModelUpdate[updated];
                   selectedPreset = name; (* Auto-select new preset *)
                ]
             ]
          ],
          Method -> "Queued",
          ImageSize -> {40, 20}
        ],
        Spacer[2],
        
        (* 4. Delete Button *)
        Button[
           Tooltip[Style["X", 10, Red], "Delete selected preset"],
           If[StringQ[selectedPreset],
              Module[{updated},
                 updated = QED`Model`DeletePreset[modelSymbol, selectedPreset];
                 onModelUpdate[updated];
                 selectedPreset = Null;
              ]
           ],
           Enabled -> Dynamic[StringQ[selectedPreset]],
           ImageSize -> {20, 20}
        ]
      }],
      (* Опции Framed *)
      FrameStyle -> LightGray,
      RoundingRadius -> 3,
      ImageMargins -> 0
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 3. CORE: QUBIT DASHBOARD *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[modelsStack : {__Association}] := DynamicModule[
  {
    currentModel = First[modelsStack],
    selectedPlotId = "PlasmonSpectrum",
    plotCache = <||>,
    performUpdate (* Вспомогательная функция для расчета *)
  },
  
  (* Определение логики обновления *)
  performUpdate = Function[{},
    (* 1. Визуальная индикация начала (покажем "Computing..." перед фризом) *)
    plotCache[selectedPlotId] = "Computing...";
    FinishDynamic[]; (* Принудительная отрисовка интерфейса перед тяжелой задачей *)
    
    (* 2. Тяжелое вычисление (происходит в потоке вызова: Button=Queued, Slider=Preemptive) *)
    Module[{res, updatedModel},
       {res, updatedModel} = ComputePlotData[selectedPlotId, currentModel];
       plotCache[selectedPlotId] = res;
       currentModel = updatedModel;
    ]
  ];

  Column[{
    Row[{
      (* LEFT PANEL: Model Selection + Controls *)
      Panel[
        Column[{
          (* Model Selector Widget *)
          SelectModel[currentModel, 
             modelsStack, 
             Function[{}, 
               (* При смене модели сбрасываем кэш *)
               plotCache = <||>;
               currentModel["Numerical", "IsDirty"] = True;
               (* Авто-расчет только для легких графиков *)
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light", performUpdate[]];
             ]
          ],
          
          Spacer[15],

          PresetControlPanel[currentModel, 
             Function[{newModel}, 
                currentModel = newModel;
                needsUpdate = True; (* Trigger re-render *)
             ]
          ],
          
          Spacer[10],
          
          (* Sliders & Button Panel *)
          PlotControlPanel[
            currentModel, 
            
            (* onSliderChange Callback *)
            Function[{}, 
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                  performUpdate[], (* Легкие графики обновляем сразу (Preemptive) *)
                  plotCache[selectedPlotId] = Missing["Stale"] (* Тяжелые помечаем как устаревшие *)
               ]
            ],
            
            (* onButtonPress Callback (Queued via PlotControlPanel definition) *)
            Function[{}, 
               currentModel["Numerical", "IsDirty"] = True; (* Форсируем пересчет *)
               performUpdate[] (* Запускаем расчет в потоке кнопки (без лимита времени) *)
            ]
          ]
        }],
        Alignment -> Top
      ],
      
      Spacer[20],
      
      (* RIGHT PANEL: Plot Area *)
      Column[{
        Row[{
           "Plot Type: ",
           (* При смене типа графика сразу запускаем расчет, если он легкий *)
           PopupMenu[Dynamic[selectedPlotId, 
             Function[{v}, 
               selectedPlotId = v; 
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light", 
                  performUpdate[],
                  (* Для тяжелых проверяем кэш, если пусто - просим нажать кнопку *)
                  If[!KeyExistsQ[plotCache, selectedPlotId], plotCache[selectedPlotId] = Missing["Init"]]
               ]
             ]], 
             Keys[$PlotRegistry]
           ]
        }],
        Spacer[10],
        
        (* 3. DISPLAY ONLY (Logic moved to Button) *)
        Dynamic[
          Switch[plotCache[selectedPlotId],
            "Computing...", 
              Panel[Column[{
                Style["Computing...", Blue, Bold],
                ProgressIndicator[Appearance -> "Indeterminate"]
              }, Alignment->Center], ImageSize->{300,300}],
            
            _Missing, 
              If[plotCache[selectedPlotId] === Missing["Stale"],
                 Panel[Style["Parameters changed. Press Update.", Gray, 16], ImageSize->{400,300}],
                 Panel[Style["Select plot or Press Update", Gray], ImageSize->{300,300}]
              ],
              
            _, plotCache[selectedPlotId]
          ],
          
          TrackedSymbols :> {plotCache, selectedPlotId}
        ]
      }, Alignment -> Top]
    }, Alignment -> Top],
    
    (* Debug Footer *)
    Dynamic @ Row[{"Cache Keys: ", Keys[plotCache]}, BaseStyle->{FontSize->10, Color->Gray}]
  }],
  
  UnsavedVariables :> {plotCache},
  Initialization :> {
    plotCache = <||>;
    (* При старте считаем график, только если он легкий *)
    If[$PlotRegistry[selectedPlotId]["Type"] === "Light", performUpdate[]];
  },
  SynchronousInitialization -> False,
  SaveDefinitions -> False
];

End[];
EndPackage[];
