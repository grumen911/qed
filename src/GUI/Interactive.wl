BeginPackage["QED`Interactive`", {"QED`Model`", "QED`Numeric`"}];

QubitDashboard::usage = "QubitDashboard[{model1, model2, ...}] launches the main interactive \
UI dashboard for exploring and visualizing superconducting circuit models.

Arguments:
  models: A list of initialized model Associations (typically created via CreateCircuitModel).

Key Features:
  * Real-Time Tuning: Adjust physical parameters via sliders with instant JIT-compiled plot updates.
  * Model Management: Seamlessly switch between multiple circuits and manage parameter presets.
  * Overlay & Export: Stack multiple plots in the overlay basket and export high-quality PDFs.
  * Drill-Down Inspector: Deep-dive into the raw, sterilized state of any model matrix or tensor.

Lifecycle:
  The dashboard automatically registers the provided models into the global $ModelRegistry \
upon initialization. When the interface is deleted or closed, it safely performs memory \
cleanup by deregistering the associated IDs.";

RegisterPlot::usage = "RegisterPlot[id, label, type, computeFunc] registers a new plot type.";

$DefaultExportPath::usage = "$DefaultExportPath specifies the default directory for saving plots. 
If the path is invalid or the directory does not exist, the system default (or last used directory) is used.";


Begin["`Private`"];


(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                  1. КОНФИГУРАЦИЯ И РЕЕСТРЫ                     ║ *)
(* ║         (Базовые настройки, пути экспорта, словари)            ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

(* Change the value below to your custom path, e.g., "C:\\Users\\Me\\Thesis\\Figures" *)
$DefaultExportPath = "C:\\Users\\rudia\\git\\2026-bic-bridge\\figures";
$PlotRegistry = <||>;
$CurrentInspectorPath = {}; (* Глобальный путь для инспектора ModelState*)

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                   2. РЕГИСТРАЦИЯ ГРАФИКОВ                      ║ *)
(* ║        (База знаний интерфейса, типы визуализаций)             ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

RegisterPlot[id_String, label_String, type_String, computeFunc_] := 
  ($PlotRegistry[id] = <|
    "Label" -> label, 
    "Type" -> type,
    "Compute" -> computeFunc
  |>);

(* ИНСПЕКТОР СОСТОЯНИЯ: Полный дамп модели *)
RegisterPlot["ModelState", "Model State Inspector", "Light",
  Function[{m, fluxR, freqR},
    Module[{displayModel},
      displayModel = KeyDrop[m, "Image"];
      displayModel = Replace[displayModel,
        {
          _CompiledFunction -> "<CompiledFunction>",
          _InterpolatingFunction -> "<InterpolatingFunction>",
          _Dispatch -> "<DispatchTable>"
        },
        {0, Infinity}
      ];
      CreateDrillDownInspector[displayModel]
    ]
  ]
];

(* Регистрация базовых графиков *)
RegisterPlot["PlasmonSpectrum", "Plasmon Spectrum", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotPlasmonSpectrum[m, FluxRange -> fluxR]]
];

RegisterPlot["Potential3D", "Potential Landscape 3D", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotPotentialSlices3D[m]] (* Пока не трогаем, он сам считает центры *)
];

RegisterPlot["WaveFunctionCheck", "Verify Harmonic Wavefunctions", "Heavy",
  Function[{m, fluxR, freqR},
    Module[{states, report, grid, nDOF},
      nDOF = m["Topology"]["DegreesOfFreedom"];
      states = {{0,0,0}, {1,0,0}, {0,1,0}, {0,0,1}};
      states = Select[states, Length[#] == nDOF &];
      If[states === {}, states = {ConstantArray[0, nDOF]}];
      
      report = Map[Function[s, QED`Numeric`VerifyWaveFunction[m, s]], states];
      Grid[Prepend[Map[Function[r, {r["State"], If[r["Status"] === "OK", Style["OK", Green, Bold], Style["FAIL", Red, Bold]], Pane[ScientificForm[r["TotalEnergy"], 5], 100], Pane[r["Norm"], 100], Pane[r["H_psi"], {250, 120}, Scrollbars -> True], Pane[r["E_psi"], {250, 120}, Scrollbars -> True]}], report], {Style["State", Bold], Style["Status", Bold], Style["Total Energy (J)", Bold], Style["Norm \[Psi]", Bold], Style["H\[Psi]", Bold], Style["E\[Psi]", Bold]}], Frame -> All, Background -> {None, {Lighter[Gray, 0.8], None}}, ItemSize -> {Automatic, 2.5}, Alignment -> {Left, Center}]
    ]
  ]
];

RegisterPlot["DiagonalizationCheck", "Verify Harmonic Diagonalization", "Light",
  Function[{m, fluxR, freqR},
    Module[{r, okStyle, failStyle, boolStyle},
      okStyle = Style["OK", Darker[Green, 0.2], Bold];
      failStyle = Style["FAIL", Red, Bold];
      boolStyle = Function[b, If[TrueQ[b], okStyle, failStyle]];

      r = QED`Numeric`VerifyDiagonalization[m];
      If[r === $Failed || FailureQ[r], Return[Panel[Style["VerifyDiagonalization failed.", Red], ImageSize -> {600, 200}]]];
      Grid[{{Style["Check", Bold], Style["Result", Bold]}, {"Is L transformed diagonal?", boolStyle[r["Is_L_Diagonal"]]}, {"Is C transformed diagonal?", boolStyle[r["Is_C_Diagonal"]]}, {"Ceff / Diagonal[N^T C N]", Pane[Short[r["EffectiveCapacitances_Check"], 3], {420, 40}, Scrollbars -> True]}, {"Transformed C = N^T C N", Pane[MatrixForm[r["Transformed_C"]], {420, 120}, Scrollbars -> True]}, {"Transformed L = N^T L^-1 N", Pane[MatrixForm[r["Transformed_L_Inverse"]], {420, 120}, Scrollbars -> True]}}, Frame -> All, Background -> {None, {Lighter[Gray, 0.8], None}}, Alignment -> {Left, Center}, ItemSize -> {Automatic, Automatic}]
    ]
  ]
];

RegisterPlot["SymbolicWaveFunction", "Inspect Symbolic Wave Function", "Light",
  Function[{m, fluxR, freqR},
    Module[{state, psiFormula, nDOF},
      nDOF = m["Topology"]["DegreesOfFreedom"];
      state = ConstantArray[0, nDOF];
      psiFormula = QED`Model`GetWaveFunction[m, state];
      If[FailureQ[psiFormula], Return["Failed to generate wavefunction."]];
      Pane[psiFormula, ImageSize -> {700, 300}, Scrollbars -> True, BaseStyle -> {LineBreakWithin -> False}]
    ]
  ]
];

RegisterPlot["SpectroscopyScanner", "Spectroscopy Scanner", "Light",
  Function[{m, fluxR, freqR}, QED`Plots`PlotSpectroscopyScanner[m]]
];

RegisterPlot["LabMatrixElements", "Matrix Elements (Lab Basis)", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotLabMatrixElements[m]]
];

RegisterPlot["FermiRates", "T1 Relaxation Times (Fermi)", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotFermiRates[m]]
];

RegisterPlot["DephasingRates", "Pure Dephasing Times (T_phi)", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotDephasingRates[m]]
];

RegisterPlot["CapacitiveRelaxationTime", "Relaxation Time (T1)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotRelaxationTime[m, FluxRange -> fluxR]]
];

RegisterPlot["InductiveRelaxationTime", "Relaxation Time (T1)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotRelaxationTime[m, "RelaxationChannel" -> "InductiveRelaxationRate", FluxRange -> fluxR]]
];

RegisterPlot["DephasingTime", "Pure Dephasing Time (T_phi)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotDephasingTime[m, FluxRange -> fluxR]]
];

RegisterPlot["Smatrix_1_2", "S-matrix (Ports 1-2)", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotFrequencyResponse[m, "Ports" -> "{1,2}", "FrequencyRange" -> freqR]]
];

RegisterPlot["Smatrix_1_4", "S-matrix (Ports 1-4)", "Light", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotFrequencyResponse[m, "Ports" -> "{1,4}", "FrequencyRange" -> freqR]]
];

RegisterPlot["SmatrixHeatmap_1_2", "S-matrix Heatmap (Ports 1-2)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotSParameterMap[m, "Ports" -> "{1,2}", FluxRange -> fluxR, "FrequencyRange" -> freqR]]
];

RegisterPlot["SmatrixHeatmap_1_4", "S-matrix Heatmap (Ports 1-4)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotSParameterMap[m, "Ports" -> "{1,4}", FluxRange -> fluxR, "FrequencyRange" -> freqR]]
];

RegisterPlot["BICCondition_1_2", "BIC Condition (Ports 1-2)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotBICModes[m, "Ports" -> "{1,2}", SweepRange -> fluxR]]
];

RegisterPlot["BICCondition_1_4", "BIC Condition (Ports 1-4)", "Heavy", 
  Function[{m, fluxR, freqR}, QED`Plots`PlotBICModes[m, "Ports" -> "{1,4}", SweepRange -> fluxR]]
];

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                   3. ВЫЧИСЛИТЕЛЬНЫЙ МОСТ                       ║ *)
(* ║     (Compute Worker, связь UI и математического ядра)          ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

ComputePlotData[plotId_String, modelId_String, fluxRange_List, freqRange_List] := 
  Module[{info, func, graphic, localModel},
    
    (* Достаем базовую модель из реестра *)
    localModel = QED`Model`GetModel[modelId];
    If[!AssociationQ[localModel], Return[Graphics[{Red, Text["Invalid Model ID"]}]]];

    (* Вызов функции отрисовки (передаем локальную копию и диапазоны!) *)
    info = $PlotRegistry[plotId];
    graphic = If[MissingQ[info], 
       Graphics[{Red, Text["Unknown Plot ID"]}],
       func = info["Compute"];
       func[localModel, fluxRange, freqRange]
    ];
    
    graphic
  ];

ExtractGraphicOnly[expr_] := Replace[expr, Legended[g_, _] :> g];

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                 4. UI КОМПОНЕНТЫ И ВИДЖЕТЫ                     ║ *)
(* ║    (Слайдеры, инспектор, пресеты, строительные блоки)          ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

(* Шаблон для хлебных крошек *)
UIInspectorCrumbTemplate[label_, isHome_: False] := Framed[
  Style[label, If[isHome, Bold, Plain], 11, RGBColor[0.2, 0.4, 0.7]],
  Background -> RGBColor[0.92, 0.95, 0.99], FrameStyle -> RGBColor[0.8, 0.85, 0.95],
  RoundingRadius -> 3, FrameMargins -> {{8, 8}, {3, 3}}
];

(* Шаблон для кнопок входа в Ассоциацию или Массив *)
UIInspectorFolderTemplate[label_] := Framed[
  Style[label, 11, Darker[Gray]], 
  Background -> RGBColor[0.95, 0.95, 0.97], FrameStyle -> RGBColor[0.85, 0.85, 0.9], 
  RoundingRadius -> 3, FrameMargins -> {{12, 12}, {5, 5}}
];

(* Основной контейнер всего инспектора *)
UIInspectorMainWrapper[content_] := Framed[
  content,
  FrameStyle -> LightGray, RoundingRadius -> 5, Background -> White,
  ImageSize -> {700, 450}, Alignment -> {Left, Top}, ImageMargins -> 5
];

(* Макет таблицы для текущего уровня *)
UIInspectorTableLayout[rows_List] := Grid[
  rows,
  Alignment -> {Left, Top},
  Dividers -> {None, Center -> LightGray},
  Spacings -> {2, 1.2}
];

(* Обёртка для навигационной панели *)
UIInspectorNavigationRow[crumbs_] := Column[{
  Row[crumbs],
  Spacer[10]
}, Alignment -> Left];

ClearAll[CreateDrillDownInspector];
CreateDrillDownInspector[rawData_Association] := 
  UIInspectorMainWrapper[
    Dynamic[
      Module[{currentData, navigation, content},
        
        (* 1. Находим данные по глобальному пути *)
        currentData = Fold[Lookup, rawData, $CurrentInspectorPath];
        
        (* 2. Готовим навигацию (Хлебные крошки), обновляя глобальный путь *)
        navigation = UIInspectorNavigationRow[
          Flatten @ Prepend[
            Table[With[{i = i}, {
              Style[" > ", Gray], 
              Button[UIInspectorCrumbTemplate[$CurrentInspectorPath[[i]]], $CurrentInspectorPath = Take[$CurrentInspectorPath, i], Appearance -> "Frameless", Cursor -> "LinkHand"]
            }], {i, 1, Length[$CurrentInspectorPath]}],
            Button[UIInspectorCrumbTemplate["Home", True], $CurrentInspectorPath = {}, Appearance -> "Frameless", Cursor -> "LinkHand"]
          ]
        ];

        (* 3. Готовим контент *)
        content = Pane[
          Switch[currentData,
            _Association,
            UIInspectorTableLayout[
              KeyValueMap[
                Function[{k, v},
                  {Style[k, Bold], 
                   Switch[v,
                     _Association, Button[UIInspectorFolderTemplate["\[RightGuillemet] Association (" <> ToString[Length[v]] <> ")"], $CurrentInspectorPath = Append[$CurrentInspectorPath, k], Appearance -> "Frameless", Cursor -> "LinkHand"],
                     _List /; Length[Flatten[v]] > 10, Button[UIInspectorFolderTemplate["\[RightGuillemet] Array " <> ToString[Dimensions[v]]], $CurrentInspectorPath = Append[$CurrentInspectorPath, k], Appearance -> "Frameless", Cursor -> "LinkHand"],
                     _String /; StringStartsQ[v, "<"], Style[v, Gray, Italic],
                     _, Pane[v, Alignment -> {Left, Top}] (* Локальное выравнивание коротких текстов *)
                   ]}
                ],
                currentData
              ]
            ],
            
            _List, MatrixForm[currentData],
            _, currentData
          ],
          
          (* Жесткие размеры ТОЛЬКО для блока данных. Оставляем место для крошек сверху *)
          ImageSize -> {680, 390}, 
          Scrollbars -> True,
          AppearanceElements -> None,
          Alignment -> {Left, Top}
        ];

        (* Собираем всё вместе *)
        Column[{navigation, content}, Alignment -> {Left, Top}]
      ],
      (* Dynamic следит только за глобальным путем *)
      TrackedSymbols :> {$CurrentInspectorPath}
    ]
  ];

ExtractInteractiveParams[model_Association] :=
  Flatten[
    KeyValueMap[
      Function[{tag, componentParams},
        KeyValueMap[
          {tag, #1, #2["Value"], {#2["Min"], #2["Max"], #2["Step"]}} &,
          Select[componentParams, AssociationQ[#] && Lookup[#, "Interactive", False] === True &]
        ]
      ],
      model["Primary"]
    ],
    1
  ];

(* Слайдер принимает ID модели и отправляет изменения прямо в Ядро *)
SetAttributes[MakeParameterControl, HoldAll];
MakeParameterControl[modelId_, {tag_, param_, val_, {min_, max_, step_}}, onUpdate_] := 
  Row[{
    Style[tag <> "." <> param <> ": ", 12],
    
    Slider[
      Dynamic[
        QED`Model`GetModel[modelId]["Primary", tag, param, "Value"], 
        Function[{v},
          QED`Model`UpdateModelParameter[modelId, tag, param, v];
          onUpdate[]
        ]
      ],
      {min, max, step},
      ImageSize -> 120
    ],
    
    Spacer[5],
    
    InputField[
      Dynamic[
        QED`Model`GetModel[modelId]["Primary", tag, param, "Value"],
        Function[{v},
          QED`Model`UpdateModelParameter[modelId, tag, param, v];
          onUpdate[]
        ]
      ],
      Number, 
      FieldSize -> {6, 1}
    ]
  }];

SetAttributes[SelectModel, HoldFirst];
SelectModel[currentModelIdSymbol_, modelIdsStack_List, onUpdate_] :=
  Row[{
    Pane[
      SetterBar[
        Dynamic[currentModelIdSymbol, 
           Function[{newId},
             currentModelIdSymbol = newId;
             onUpdate[]; 
           ]
        ],
        (# -> Tooltip[
                 Show[QED`Model`GetModel[#]["Image"], ImageSize->{60,60}, AspectRatio->1, Axes->False, Frame->True, FrameTicks->None], 
                 Lookup[QED`Model`GetModel[#]["Topology"], "Name", "Circuit"]
              ]) & /@ modelIdsStack,
        Appearance -> "Vertical"
      ],
      ImageSize -> {80, 200},
      Scrollbars -> {False, True}
    ],
    Spacer[10],
    
    Dynamic[
      Module[{m = QED`Model`GetModel[currentModelIdSymbol]},
        Column[{
          Style[Lookup[m["Topology"], "Name", "Circuit"], Bold, 12],
          Show[m["Image"], ImageSize -> {180, 180}, AspectRatio->1]
        }, Alignment -> Center]
      ],
      TrackedSymbols :> {currentModelIdSymbol}
    ]
  }];

SetAttributes[PlotControlPanel, HoldAll];
PlotControlPanel[modelIdSymbol_, onUpdate_, onForceUpdate_, triggerSymbol_] := 
  Dynamic[
    Module[{m = QED`Model`GetModel[modelIdSymbol], params},
      If[!AssociationQ[m], Return[""]];
      params = ExtractInteractiveParams[m];
      Column[
        Join[
          Map[MakeParameterControl[modelIdSymbol, #, onUpdate] &, params],
          {Spacer[10],
           Button["Update Plot", 
             onForceUpdate[],
             Method -> "Queued",
             ImageSize -> {140, 30}
           ]}
        ]
      ]
    ],
    TrackedSymbols :> {modelIdSymbol, triggerSymbol} 
  ];

SetAttributes[PresetControlPanel, HoldAll];
PresetControlPanel[modelIdSymbol_, onModelUpdate_] := 
  DynamicModule[{selectedPreset = Null, getModelKey, hamburgerIcon},
    
    getModelKey[id_] := Lookup[QED`Model`GetModel[id]["Topology"], "Name", "DefaultCircuit"];
    
    hamburgerIcon = Graphics[
      {GrayLevel[0.4], CapForm["Round"], Thickness[0.15], 
       Line[{{0, 0.25}, {1, 0.25}}], Line[{{0, 0.5}, {1, 0.5}}], Line[{{0, 0.75}, {1, 0.75}}]}, 
      ImageSize -> {12, 12}, PlotRange -> {{0, 1}, {0, 1}}, ImagePadding -> 0, BaselinePosition -> Center
    ];

    Framed[
      Row[{
        Style["Presets: ", 10, Gray],
        
        Dynamic[
          PopupMenu[
            Dynamic[selectedPreset],
            QED`Model`GetPresetNames[modelIdSymbol], 
            "Select...",
            ImageSize -> {90, Automatic}
          ]
        ],
        Spacer[5],
        
        Button[
          Tooltip[Style["Load", 10], "Load selected preset"],
          If[StringQ[selectedPreset],
             QED`Model`LoadPreset[modelIdSymbol, selectedPreset];
             onModelUpdate[];
          ],
          Enabled -> Dynamic[StringQ[selectedPreset]],
          ImageSize -> {40, 20}
        ],
        Spacer[2],
        
        Button[
          Tooltip[Style["Save", 10], "Save current configuration"],
          Module[{name},
             name = DialogInput[{text = ""}, 
                Column[{
                  Style["Save Preset", Bold],
                  InputField[Dynamic[text], String],
                  Row[{DefaultButton["Save", DialogReturn[text]], CancelButton[]}]
                }]
             ];
             If[StringQ[name] && StringLength[name] > 0,
                QED`Model`SavePreset[modelIdSymbol, name];
                selectedPreset = name;
                onModelUpdate[];
             ]
          ],
          Method -> "Queued",
          ImageSize -> {40, 20}
        ],
        Spacer[2],
        
        Button[
           Tooltip[Style["X", 10, Red], "Delete selected preset"],
           If[StringQ[selectedPreset],
              QED`Model`DeletePreset[modelIdSymbol, selectedPreset];
              selectedPreset = Null;
              onModelUpdate[];
           ],
           Enabled -> Dynamic[StringQ[selectedPreset]],
           ImageSize -> {20, 20}
        ],
        
        Spacer[10],
        
        ActionMenu[
           Tooltip[MouseAppearance[Pane[hamburgerIcon, ImageSize -> {20, 20}, Alignment -> Center], "LinkHand"], "Notebook Storage Options"],
           {
             "Save to Notebook..." :> Module[{key},
                key = getModelKey[modelIdSymbol];
                If[ChoiceDialog[
                     "Overwrite preset metadata in this notebook?\nExisting presets for this model in the file metadata will be replaced.",
                     {"Overwrite" -> True, "Cancel" -> False},
                     WindowTitle -> "Confirm Save to Notebook"
                   ],
                   CurrentValue[EvaluationNotebook[], {TaggingRules, "QED_Presets", key}] = QED`Model`GetModel[modelIdSymbol]["Presets"];
                ]
             ],
             "Merge from Notebook" :> Module[{key, saved},
                key = getModelKey[modelIdSymbol];
                saved = CurrentValue[EvaluationNotebook[], {TaggingRules, "QED_Presets", key}];
                If[AssociationQ[saved],
                   QED`Model`MergePresets[modelIdSymbol, saved];
                   onModelUpdate[];
                ]
             ]
           },
           Appearance -> "None", ImageSize -> {20, 20}, Method -> "Queued"
        ]
      }],
      FrameStyle -> LightGray, RoundingRadius -> 3, ImageMargins -> 0
    ]
  ];

(* Виджет для управления интервалами (Flux / Frequency) *)
SetAttributes[MakeIntervalControl, HoldAll];
MakeIntervalControl[label_, symbol_, {minLimit_, maxLimit_, step_}, onUpdate_] := 
  Column[{
    Style[label, 11, Bold, GrayLevel[0.3]],
    Row[{
      InputField[
        Dynamic[symbol[[1]], Function[{v}, symbol = {Min[v, symbol[[2]] - step], symbol[[2]]}; onUpdate[]]], 
        Number, FieldSize -> {4, 1}
      ],
      Spacer[5],
      IntervalSlider[
        Dynamic[symbol, Function[{v}, symbol = v; onUpdate[]]], 
        {minLimit, maxLimit, step}, 
        ImageSize -> 120, MinIntervalSize -> step
      ],
      Spacer[5],
      InputField[
        Dynamic[symbol[[2]], Function[{v}, symbol = {symbol[[1]], Max[v, symbol[[1]] + step]}; onUpdate[]]], 
        Number, FieldSize -> {4, 1}
      ]
    }]
  }, Alignment -> Left];

makeGearIcon[color_] := Graphics[{color, Disk[{0, 0}, 0.7], Table[Rotate[{EdgeForm[None], Rectangle[{-0.15, 0.6}, {0.15, 0.95}]}, ang, {0, 0}], {ang, 0, 2 Pi - 0.1, Pi/4}], White, Disk[{0, 0}, 0.3]}, ImageSize -> 18, PlotRange -> {{-1, 1}, {-1, 1}}, BaselinePosition -> Center];

(* ╔════════════════════════════════════════════════════════════════╗ *)
(* ║                     5. ГЛАВНЫЙ ДАШБОРД                         ║ *)
(* ║      (QubitDashboard, управление состоянием и рендеринг)       ║ *)
(* ╚════════════════════════════════════════════════════════════════╝ *)

QubitDashboard[modelsStack : {__Association}] := DynamicModule[
  {
    modelIds = QED`Model`RegisterModel /@ modelsStack,
    currentModelId,
    uiTick = 1,
    selectedPlotId = "PlasmonSpectrum",
    plotCache = <||>,
    overlayBasket = <||>,
    showExportSettings = False,
    exportPreset = "Publication",
    performUpdate,
    
    isComputing = False,

    globalFluxRange = {0.0, 0.5},
    globalFreqRange = {0.0, 20.0}
  },
  
  performUpdate = Function[{},
    If[isComputing, Return[]]; (* Если уже считаем - игнорируем новые запросы *)
    
    isComputing = True;
    
    (* Показываем лоадер ТОЛЬКО для тяжелых графиков *)
    If[$PlotRegistry[selectedPlotId]["Type"] === "Heavy",
      plotCache[selectedPlotId] = "Computing...";
      uiTick++;
      FinishDynamic[]; (* Принудительно заставляем UI нарисовать заглушку *)
    ];
    
    (* Вызываем Compute *)
    Module[{newData},
      (* ТЕПЕРЬ ПЕРЕДАЕМ ДИАПАЗОНЫ *)
      newData = ComputePlotData[selectedPlotId, currentModelId, globalFluxRange, globalFreqRange];
      plotCache = Association[plotCache, selectedPlotId -> newData];
    ];
    
    isComputing = False;
    uiTick++;
  ];

  Column[{
    Row[{
      (* LEFT PANEL *)
      Panel[
        Column[{
          SelectModel[currentModelId, modelIds, 
            Function[{}, 
              plotCache = <||>;
              $CurrentInspectorPath = {}; (* сброс пути при смене модели *)
              If[$PlotRegistry[selectedPlotId]["Type"] === "Light", performUpdate[]];
            ]
          ],
          Spacer[15],
          PresetControlPanel[currentModelId, 
             Function[{}, 
               performUpdate[];
               uiTick++; (* Дергаем триггер, чтобы обновились ползунки на экране *)
             ]
          ],
          Spacer[10],
          PlotControlPanel[currentModelId, 
            Function[{}, 
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                 (* Выбрасываем ключ из словаря ДО начала расчета *)
                 plotCache = KeyDrop[plotCache, selectedPlotId];
                 performUpdate[], 
                 
                 (* Записываем Stale через создание новой ассоциации *)
                 plotCache = Association[plotCache, selectedPlotId -> Missing["Stale"]]
               ];
               uiTick++; 
            ],
            Function[{}, performUpdate[]],
            uiTick
          ],

          Spacer[15],
          
          (* --- НОВЫЙ БЛОК: SWEEP DOMAINS --- *)
          Framed[
            Column[{
              Style["Sweep Domains", Bold, 11, GrayLevel[0.5]],
              Spacer[5],
              MakeIntervalControl["External Flux (\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(ext\)]\)/\!\(\*SubscriptBox[\(\[CapitalPhi]\), \(0\)]\)):", globalFluxRange, {0.0, 0.5, 0.01}, 
                Function[{}, 
                  If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                    plotCache = KeyDrop[plotCache, selectedPlotId];
                    performUpdate[], 
                    plotCache = Association[plotCache, selectedPlotId -> Missing["Stale"]]
                  ];
                  uiTick++;
                ]
              ],
              Spacer[10],
              MakeIntervalControl["Frequency (GHz):", globalFreqRange, {0.0, 40.0, 0.1}, 
                Function[{}, 
                  If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                    plotCache = KeyDrop[plotCache, selectedPlotId];
                    performUpdate[], 
                    plotCache = Association[plotCache, selectedPlotId -> Missing["Stale"]]
                  ];
                  uiTick++;
                ]
              ]
            }],
            FrameStyle -> LightGray, RoundingRadius -> 3, Background -> White, 
            ImageMargins -> 0, FrameMargins -> 10
          ]
          (* --------------------------------- *)
        }],
        Alignment -> Top
      ],
      
      Spacer[20],
      
      (* RIGHT PANEL: Plot Area *)
      Column[{
        (* 1. UNIFIED TOOLBAR *)
        Row[{
           "Plot Type: ",
           PopupMenu[Dynamic[selectedPlotId, 
             Function[{v}, 
               selectedPlotId = v;
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light", 
                  performUpdate[],
                  If[!KeyExistsQ[plotCache, selectedPlotId], plotCache[selectedPlotId] = Missing["Init"]]
               ];
               uiTick++;
             ]], 
             Keys[$PlotRegistry]
           ],
           
           Spacer[20],
           
           Button["Add to Overlay",
             Module[{curr = plotCache[selectedPlotId]},
               If[!MissingQ[curr] && !FailureQ[curr],
                  If[!KeyExistsQ[overlayBasket, selectedPlotId], overlayBasket[selectedPlotId] = {}];
                  AppendTo[overlayBasket[selectedPlotId], ExtractGraphicOnly[curr]];
                  uiTick++;
               ]
             ],
             Enabled -> Dynamic[MatchQ[plotCache[selectedPlotId], _Graphics | _Legended]],
             ImageSize -> {100, Automatic}
           ],
           
           Spacer[5],
           
           Button["Clear",
             overlayBasket[selectedPlotId] = {}; uiTick++,
             Enabled -> Dynamic[Length[Lookup[overlayBasket, selectedPlotId, {}]] > 0],
             ImageSize -> {50, Automatic}
           ],
           
           Spacer[5],
           Dynamic[Style["(" <> ToString[Length[Lookup[overlayBasket, selectedPlotId, {}]]] <> ")", Gray], TrackedSymbols :> {uiTick}],
           
           Spacer[30], 
           
           (* C. SAVE & EXPORT CONTROLS *)
           Button[
              Row[{Style["Save PDF...", Bold], Spacer[5], Style["\[DownArrow]", Gray]}],
              Module[{targetFile, gToSave, finalG, savedOverlays, initialPath, safePath},
                 safePath = $DefaultExportPath;
                 initialPath = If[StringQ[safePath] && DirectoryQ[safePath], FileNameJoin[{safePath, "plot.pdf"}], "plot.pdf"];
                 targetFile = SystemDialogInput["FileSave", initialPath];
                 If[StringQ[targetFile],
                    savedOverlays = Lookup[overlayBasket, selectedPlotId, {}];
                    gToSave = If[Length[savedOverlays] > 0, Show[Join[savedOverlays, {plotCache[selectedPlotId]}], PlotRange->All], plotCache[selectedPlotId]];
                    finalG = QED`Style`ApplyExportPreset[gToSave, exportPreset];
                    Check[Export[targetFile, finalG, "PDF"]; Beep[], Beep[]; Beep[]]
                 ];
              ],
              Method -> "Queued", ImageSize -> {110, Automatic}
           ],
           Spacer[5],
           Button[MouseAppearance[makeGearIcon[If[showExportSettings, Darker[Blue], Gray]], "LinkHand"], showExportSettings = !showExportSettings, Appearance -> "Frameless", ImageSize -> {22, 22}]
        }],
        
        (* 2. DRAWER *)
        Pane[
           Dynamic[
               If[showExportSettings,
                  Framed[Column[{Style["Export Settings", Bold, 10], Spacer[5], Row[{"Preset: ", PopupMenu[Dynamic[exportPreset], {"Screen" -> "Screen (WYSIWYG)", "Publication" -> "Publication (Thick Lines, Arial)"}]}], Spacer[5], Text[Style["Tip: 'Publication' scales lines and fonts\nfor Illustrator editing.", Gray, 8]]}], FrameStyle -> LightGray, Background -> Lighter[Gray, 0.95], RoundingRadius -> 4, ImageMargins -> {{0,0}, {5,5}}],
                  Spacer[0]
               ]
           ],
           ImageSize -> {Automatic, Automatic}, ImageSizeAction -> "ShrinkToFit", Alignment -> Left
        ],
        Spacer[10],
        
        (* 3. DISPLAY AREA - ОБНОВЛЕННЫЙ *)
        Dynamic[
          Module[{curr, saved, finalDisplay},
            
            (* Читаем напрямую из кэша. Dynamic сам отследит изменения plotCache *)
            curr = Lookup[plotCache, selectedPlotId, Missing["Init"]];
            saved = Lookup[overlayBasket, selectedPlotId, {}];
            
            (* Собираем то, что должно быть на экране *)
            finalDisplay = Switch[curr,
              "Computing...", Panel[Column[{Style["Computing...", Blue, Bold], ProgressIndicator[Appearance -> "Indeterminate"]}, Alignment->Center], ImageSize->{300,300}],
              _Missing, If[curr === Missing["Stale"], Panel[Style["Parameters changed. Press Update.", Gray, 16], ImageSize->{400,300}], Panel[Style["Select plot or Press Update", Gray], ImageSize->{300,300}]],
              _, If[Length[saved] > 0 && (MatchQ[curr, _Graphics] || MatchQ[curr, _Legended]), Show[Join[saved, {curr}], PlotRange -> All], curr]
            ];
            
            (* Привязываем значение uiTick прямо к объекту, 
               чтобы 100% заставить FrontEnd перерисовать пиксели *)
            Style[finalDisplay, "RenderTrigger" -> uiTick]
          ]
        ]
      }, Alignment -> Top]
    }, Alignment -> Top],
    
    Dynamic @ Row[{"Render Tick: ", uiTick, " | Active ID: ", StringTake[currentModelId, -6]}, BaseStyle->{FontSize->10, Color->Gray}]
  }],
  
  Initialization :> (
    currentModelId = First[modelIds];

    (* Автоматический мердж пресетов из блокнота для всех загруженных моделей *)
    Scan[
      Function[id,
        Module[{key, saved},
          key = Lookup[QED`Model`GetModel[id]["Topology"], "Name", "DefaultCircuit"];
          saved = CurrentValue[EvaluationNotebook[], {TaggingRules, "QED_Presets", key}];
          If[AssociationQ[saved],
            QED`Model`MergePresets[id, saved];
          ];
        ]
      ],
      modelIds
    ];

    If[$PlotRegistry[selectedPlotId]["Type"] === "Light", performUpdate[]];
  ),
  Deinitialization :> (
    (* Удаляем модели из реестра при закрытии окна *)
    QED`Model`Private`$ModelRegistry = KeyDrop[QED`Model`Private`$ModelRegistry, modelIds];
  ),
  SynchronousInitialization -> False
];

End[];
EndPackage[];