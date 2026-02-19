BeginPackage["QED`Numeric`Calculators`"];


CalcCompiledEngines::usage = "CalcCompiledEngines[analytical, fluxSymbols, paramSymbols] \
generates JIT-compiled C-functions {FastGrad, FastHess} for root finding.";

CalcStaticMatrices::usage = "CalcStaticMatrices[analytical, rules] computes the static \
numerical capacitance matrix and its inverse. Returns {C_num, InvC_num}.";

CalcEquilibrium::usage = "CalcEquilibrium[compiledEngines, guess, paramVector] performs \
a fast local Newton search for equilibrium flux using JIT engines.";

Begin["`Private`"];


CalcCompiledEngines[analytical_Association, fluxSymbols_List, paramSymbols_List] := Module[
  {
    phi0, energyScale, constantRules, 
    potSym, potRescaled, gradSym, hessSym, 
    fluxRules, paramRules, allRules,
    heldGrad, heldHess, fastGrad, fastHess
  },
  
  (* 1. Определяем масштабы и константы *)
  phi0 = QED`$Phi0Value;
  energyScale = QED`$hbarValue * 2 * Pi * 10^9; (* Энергия фотона 1 ГГц в Джоулях *)
  
  constantRules = {
    QED`$Phi0 -> phi0,
    QED`$hbar -> QED`$hbarValue,
    QED`$e   -> QED`$eValue
  };
  
  (* 2. Извлекаем сырой потенциал и подставляем константы *)
  potSym = analytical["Potential"] /. constantRules;
  
  (* 3. Символьное обезразмеривание: 
     Делим энергию на масштаб, фазы заменяем на (безразмерная_фаза * Phi0) *)
  potRescaled = (potSym / energyScale) /. 
    Table[fluxSymbols[[i]] -> fluxSymbols[[i]] * phi0, {i, Length[fluxSymbols]}];

    Print["[Debug] Rescaled Potential: ", potRescaled];
  
  (* 4. Берем производные по БЕЗРАЗМЕРНЫМ переменным *)
  gradSym = D[potRescaled, {fluxSymbols}];
  hessSym = D[gradSym, {fluxSymbols}];
  
  (* 5. Формируем правила для JIT-трансляции *)
  fluxRules = Table[With[{idx = i}, fluxSymbols[[idx]] :> phi[[idx]]], {i, Length[fluxSymbols]}];
  paramRules = Table[With[{idx = i}, paramSymbols[[idx]] :> p[[idx]]], {i, Length[paramSymbols]}];
  allRules = Join[fluxRules, paramRules];
  
  (* 6. Защита в Hold *)
  heldGrad = With[{g = gradSym}, Hold[g]] /. allRules;
  heldHess = With[{h = hessSym}, Hold[h]] /. allRules;
  
  (* 7. Компиляция в C с оптимизацией выражений *)
  fastGrad = heldGrad /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}},
    body,
    CompilationTarget -> "C",
    RuntimeOptions -> "Speed",
    CompilationOptions -> {"ExpressionOptimization" -> True, "InlineExternalDefinitions" -> True}
  ];
  
  fastHess = heldHess /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}},
    body,
    CompilationTarget -> "C",
    RuntimeOptions -> "Speed",
    CompilationOptions -> {"ExpressionOptimization" -> True, "InlineExternalDefinitions" -> True}
  ];
  
  {fastGrad, fastHess}
];

CalcStaticMatrices[analytical_Association, rules_List] := Module[
  {capSym, cNum, invCNum},
  
  (* Извлекаем символьную матрицу *)
  capSym = analytical["CapacitanceMatrix"];
  
  (* Подставляем правила и приводим к машинным числам (Real) *)
  cNum = N[capSym /. rules];
  
  (* Безопасное обращение матрицы *)
  invCNum = If[Det[cNum] != 0, 
    Inverse[cNum], 
    $Failed
  ];
  
  {cNum, invCNum}
];

CalcEquilibrium[{fastGrad_, fastHess_}, guess_List, paramVector_?Developer`PackedArrayQ] := Module[
  {fg, fh, guessDimless, phiVec, root, phiMinDimless},
  
  (* 1. Обезразмериваем стартовую точку (Веберы -> Радианы/2Pi) *)
  guessDimless = guess / QED`$Phi0Value;
  
  (* 2. Защита от символьного вычисления *)
  fg[v_?(VectorQ[#, NumericQ] &)] := fastGrad[v, paramVector];
  fh[v_?(VectorQ[#, NumericQ] &)] := fastHess[v, paramVector];
  
  (* 3. Вызов метода Ньютона в безразмерных координатах *)
  root = Quiet[
    FindRoot[
      fg[phiVec],
      {phiVec, guessDimless},
      Jacobian -> fh[phiVec],
      Method -> "Newton"
    ],
    {FindRoot::cvmit, FindRoot::lstol, FindRoot::jsing}
  ];
  
  (* 4. Извлекаем безразмерный результат *)
  phiMinDimless = If[root === $Failed, 
    $Failed, 
    phiVec /. root
  ];
  
  (* 5. Возвращаем размерный результат (Веберы), упаковывая в C-массив *)
  If[phiMinDimless =!= $Failed,
    Developer`ToPackedArray[phiMinDimless * QED`$Phi0Value, Real],
    $Failed
  ]
];

End[];
EndPackage[];