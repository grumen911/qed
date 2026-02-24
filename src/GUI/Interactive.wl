BeginPackage["QED`Interactive`", {"QED`Model`", "QED`Numeric`"}];

QubitDashboard::usage = "QubitDashboard[{models..}] - interactive dashboard for model list.";
RegisterPlot::usage = "RegisterPlot[id, label, type, computeFunc] registers a new plot type.";

$DefaultExportPath::usage = "$DefaultExportPath specifies the default directory for saving plots. 
If the path is invalid or the directory does not exist, the system default (or last used directory) is used.";

Begin["`Private`"];

(* === USER CONFIGURATION === *)
(* Change the value below to your custom path, e.g., "C:\\Users\\Me\\Thesis\\Figures" *)
$DefaultExportPath = "C:\\Users\\rudia\\git\\2026-bic-bridge\\figures";


(* ================================================================= *)
(* UI COMPONENT: DRILL-DOWN INSPECTOR (WITH STERILIZATION)           *)
(* ================================================================= *)

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

(* 1. Основной контейнер всего инспектора *)
UIInspectorMainWrapper[content_] := Framed[
  content,
  FrameStyle -> LightGray, RoundingRadius -> 5, Background -> White,
  ImageSize -> {700, 450}, Alignment -> {Left, Top}, ImageMargins -> 5
];

(* 2. Макет таблицы для текущего уровня *)
UIInspectorTableLayout[rows_List] := Grid[
  rows,
  Alignment -> {Left, Top},
  Dividers -> {None, Center -> LightGray},
  Spacings -> {2, 1.2}
];

(* 3. Обёртка для навигационной панели *)
UIInspectorNavigationRow[crumbs_] := Column[{
  Row[crumbs],
  Spacer[10]
}, Alignment -> Left];

(* Глобальная переменная для хранения пути инспектора *)
$CurrentInspectorPath = {};

ClearAll[CreateDrillDownInspector];
(* Убрали HoldFirst, так как теперь передаем чистую Association *)

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
        content = Switch[currentData,
          _Association,
          UIInspectorTableLayout[
            KeyValueMap[
              Function[{k, v},
                {Style[k, Bold], 
                 Switch[v,
                   _Association, 
                   Button[UIInspectorFolderTemplate["\[RightGuillemet] Association (" <> ToString[Length[v]] <> ")"], $CurrentInspectorPath = Append[$CurrentInspectorPath, k], Appearance -> "Frameless", Cursor -> "LinkHand"],
                   
                   _List /; Length[Flatten[v]] > 10, 
                   Button[UIInspectorFolderTemplate["\[RightGuillemet] Array " <> ToString[Dimensions[v]]], $CurrentInspectorPath = Append[$CurrentInspectorPath, k], Appearance -> "Frameless", Cursor -> "LinkHand"],
                   
                   _String /; StringStartsQ[v, "<"], Style[v, Gray, Italic],
                   _, Pane[v, Alignment -> {Left, Top}]
                 ]}
              ],
              currentData
            ]
          ],
          
          _List, Pane[MatrixForm[currentData], {650, 350}, Scrollbars -> True],
          _, Pane[currentData]
        ];

        (* Собираем всё вместе *)
        Column[{navigation, content}, Alignment -> {Left, Top}]
      ],
      (* Dynamic следит только за глобальным путем *)
      TrackedSymbols :> {$CurrentInspectorPath}
    ]
  ];

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

(* ИНСПЕКТОР СОСТОЯНИЯ: Полный дамп модели *)
RegisterPlot["ModelState", "Model State Inspector", "Light",
  Function[{m},
    Module[{displayModel},
      
      (* 1. Убираем картинку схемы *)
      displayModel = KeyDrop[m, "Image"];

      (* 2. СТЕРИЛИЗАЦИЯ: убираем токсичные бинарные объекты Ядра перед отправкой в UI *)
      displayModel = Replace[displayModel,
        {
          _CompiledFunction -> "<CompiledFunction>",
          _InterpolatingFunction -> "<InterpolatingFunction>",
          _Dispatch -> "<DispatchTable>"
        },
        {0, Infinity}
      ];

      (* 3. Оборачиваем в инспектор *)
      Pane[
        CreateDrillDownInspector[displayModel],
        ImageSize -> {700, 450}, 
        Scrollbars -> True,
        AppearanceElements -> None
      ]
    ]
  ]
];

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

RegisterPlot["DephasingRates", "Pure Dephasing Times (T_phi)", "Light", 
  Function[{m}, QED`Plots`PlotDephasingRates[m]]
];

RegisterPlot["CapacitiveRelaxationTime", "Relaxation Time (T1)", "Heavy", 
  Function[{m}, QED`Plots`PlotRelaxationTime[m]]
];

RegisterPlot["InductiveRelaxationTime", "Relaxation Time (T1)", "Heavy", 
  Function[{m}, QED`Plots`PlotRelaxationTime[m, "RelaxationChannel" -> "InductiveRelaxationRate"]]
];

RegisterPlot["DephasingTime", "Pure Dephasing Time (T_phi)", "Heavy", 
  Function[{m}, QED`Plots`PlotDephasingTime[m]]
];

RegisterPlot["Smatrix", "Scattering Parameters (S-matrix)", "Light", 
  Function[{m}, QED`Plots`PlotFrequencyResponse[m, {0.1, 20.}]]
];

RegisterPlot["SmatrixHeatmap", "Scattering Parameters Heatmap", "Heavy", 
  Function[{m}, QED`Plots`PlotSParameterMap[m, {0.1, 20.}]]
];

(* 
   COMPUTE WORKER (FUNCTIONAL STYLE)
   Input: plotId, model (Value)
   Output: {Graphics, UpdatedModel (Value)}
*)
(* COMPUTE WORKER (HANDLE-BASED) *)
(* Теперь принимает не ассоциацию, а строковый ID модели *)
ComputePlotData[plotId_String, modelId_String] := 
  Module[{info, func, graphic, localModel},
    
    (* 1. Достаем базовую модель из реестра *)
    localModel = QED`Model`GetModel[modelId];
    If[!AssociationQ[localModel], Return[Graphics[{Red, Text["Invalid Model ID"]}]]];

    (* 2. Прогрев кэша (JIT теперь сам безопасно обновляет $ModelRegistry по ModelID) *)
    QED`Model`GetNumericalQuantity[localModel, "PlasmonFrequencies"];
    
    (* Примечание: в новом реестре ключ называется EquilibriumFluxes *)
    If[plotId === "Potential3D", QED`Model`GetNumericalQuantity[localModel, "EquilibriumFluxes"]];
    If[plotId === "DiagonalizationCheck", QED`Model`GetNumericalQuantity[localModel, "HarmonicDiagonalization"]];

    (* 3. Забираем СВЕЖУЮ модель из реестра (уже с прогретым кэшем) *)
    localModel = QED`Model`GetModel[modelId];

    (* 4. Вызов функции отрисовки (передаем чистую локальную копию!) *)
    info = $PlotRegistry[plotId];
    graphic = If[MissingQ[info], 
       Graphics[{Red, Text["Unknown Plot ID"]}],
       func = info["Compute"];
       func[localModel]
    ];
    
    graphic
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
          Select[componentParams, AssociationQ[#] && Lookup[#, "Interactive", False] === True &]
        ]
      ],
      model["Primary"]
    ],
    1
  ];

(* Слайдер теперь принимает ID модели и отправляет изменения прямо в Ядро *)
SetAttributes[MakeParameterControl, HoldFirst];
MakeParameterControl[modelId_, {tag_, param_, val_, {min_, max_, step_}}, onUpdate_, isComputingSymbol_] := 
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
      ImageSize -> 120,
      Enabled -> Dynamic[!TrueQ[isComputingSymbol]] (* Блокировка слайдера *)
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
      FieldSize -> {6, 1},
      Enabled -> Dynamic[!TrueQ[isComputingSymbol]] (* Блокировка поля ввода *)
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

SetAttributes[PlotControlPanel, HoldFirst];
PlotControlPanel[modelIdSymbol_, onUpdate_, onForceUpdate_, triggerSymbol_, isComputingSymbol_] := 
  Dynamic[
    Module[{m = QED`Model`GetModel[modelIdSymbol], params},
      If[!AssociationQ[m], Return[""]];
      params = ExtractInteractiveParams[m];
      Column[
        Join[
          Map[MakeParameterControl[modelIdSymbol, #, onUpdate, isComputingSymbol] &, params],
          {Spacer[10],
           Button["Update Plot", 
             onForceUpdate[],
             Method -> "Queued",
             ImageSize -> {140, 30},
             Enabled -> Dynamic[!TrueQ[isComputingSymbol]] (* Блокировка кнопки *)
           ]}
        ]
      ]
    ],
    TrackedSymbols :> {modelIdSymbol, triggerSymbol} 
  ];

SetAttributes[PresetControlPanel, HoldFirst];
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

makeGearIcon[color_] := Graphics[{color, Disk[{0, 0}, 0.7], Table[Rotate[{EdgeForm[None], Rectangle[{-0.15, 0.6}, {0.15, 0.95}]}, ang, {0, 0}], {ang, 0, 2 Pi - 0.1, Pi/4}], White, Disk[{0, 0}, 0.3]}, ImageSize -> 18, PlotRange -> {{-1, 1}, {-1, 1}}, BaselinePosition -> Center];
ExtractGraphicOnly[expr_] := Replace[expr, Legended[g_, _] :> g];

(* ═══════════════════════════════════════════════════════════════ *)
(* 3. CORE: QUBIT DASHBOARD (TRIGGER-BASED) *)
(* ═══════════════════════════════════════════════════════════════ *)

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
    
    isComputing = False (* НОВЫЙ ФЛАГ СОСТОЯНИЯ *)
  },
  
  performUpdate = Function[{},
    (* Если уже считаем - игнорируем новые запросы *)
    If[isComputing, Return[]]; 
    
    isComputing = True;
    
    (* Показываем лоадер ТОЛЬКО для тяжелых графиков *)
    If[$PlotRegistry[selectedPlotId]["Type"] === "Heavy",
      plotCache[selectedPlotId] = "Computing...";
      uiTick++;
      FinishDynamic[]; (* Принудительно заставляем UI нарисовать заглушку *)
    ];
    
    (* Вызываем Compute (для Light он выполнится за миллисекунды) *)
    plotCache[selectedPlotId] = ComputePlotData[selectedPlotId, currentModelId];
    
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
              $CurrentInspectorPath = {}; (* <--- СБРОС ПУТИ ПРИ СМЕНЕ КУБИТА *)
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
                  performUpdate[], 
                  plotCache[selectedPlotId] = Missing["Stale"] 
               ];
               uiTick++; (* Приказываем перерисовать график! *)
            ],
            Function[{}, performUpdate[]],
            uiTick,
            isComputing
          ]
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
           
           (* C. SAVE & EXPORT CONTROLS (без изменений) *)
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
        
        (* 3. DISPLAY AREA - ТЕПЕРЬ СТРОГО СЛЕДИТ ЗА ТРИГГЕРОМ *)
        Dynamic[
          Refresh[
            Module[{curr, saved},
              curr = plotCache[selectedPlotId];
              saved = Lookup[overlayBasket, selectedPlotId, {}];
              Switch[curr,
                "Computing...", Panel[Column[{Style["Computing...", Blue, Bold], ProgressIndicator[Appearance -> "Indeterminate"]}, Alignment->Center], ImageSize->{300,300}],
                _Missing, If[curr === Missing["Stale"], Panel[Style["Parameters changed. Press Update.", Gray, 16], ImageSize->{400,300}], Panel[Style["Select plot or Press Update", Gray], ImageSize->{300,300}]],
                _, If[Length[saved] > 0 && (MatchQ[curr, _Graphics] || MatchQ[curr, _Legended]), Show[Join[saved, {curr}], PlotRange -> All], curr]
              ]
            ],
            TrackedSymbols :> {uiTick, selectedPlotId, currentModelId}
          ]
        ]
      }, Alignment -> Top]
    }, Alignment -> Top],
    
    Dynamic @ Row[{"Render Tick: ", uiTick, " | Active ID: ", StringTake[currentModelId, -6]}, BaseStyle->{FontSize->10, Color->Gray}]
  }],
  
  Initialization :> (
    currentModelId = First[modelIds];
    If[$PlotRegistry[selectedPlotId]["Type"] === "Light", performUpdate[]];
  ),
  Deinitialization :> (
    (* ОСВОБОЖДЕНИЕ ПАМЯТИ: удаляем модели из реестра при закрытии окна *)
    QED`Model`Private`$ModelRegistry = KeyDrop[QED`Model`Private`$ModelRegistry, modelIds];
  ),
  SynchronousInitialization -> False
];

End[];
EndPackage[];