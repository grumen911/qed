BeginPackage["QED`Numeric`Calculators`"];


CalcCompiledEngines::usage = "CalcCompiledEngines[analytical, fluxSymbols, paramSymbols] \
generates JIT-compiled C-functions {FastGrad, FastHess} for root finding.";

CalcStaticMatrices::usage = "CalcStaticMatrices[analytical, rules] computes the static \
numerical capacitance matrix and its inverse. Returns {C_num, InvC_num}.";


Begin["`Private`"];


CalcCompiledEngines[analytical_Association, fluxSymbols_List, paramSymbols_List] := Module[
  {
    constantRules, gradSym, hessSym, 
    fluxRules, paramRules, allRules,
    heldGrad, heldHess, fastGrad, fastHess
  },
  
  (* 1. Выделяем физические константы для пред-подстановки *)
  constantRules = {
    QED`$Phi0 -> QED`$Phi0Value,
    QED`$hbar -> QED`$hbarValue,
    QED`$e   -> QED`$eValue
  };
  
  (* Извлекаем аналитику и сразу подставляем константы (они не пойдут в вектор p) *)
  gradSym = analytical["PotentialGradient"] /. constantRules;
  hessSym = (D[analytical["PotentialGradient"], {fluxSymbols}]) /. constantRules;
  
  (* 2. БЕЗОПАСНЫЕ ПРАВИЛА (Лексическое замыкание)
     Используем With для фиксации конкретного числа idx, 
     чтобы внутрь RuleDelayed не утекла локальная переменная цикла. *)
  fluxRules = Table[
    With[{idx = i}, fluxSymbols[[idx]] :> phi[[idx]]], 
    {i, Length[fluxSymbols]}
  ];
  paramRules = Table[
    With[{idx = i}, paramSymbols[[idx]] :> p[[idx]]], 
    {i, Length[paramSymbols]}
  ];
  allRules = Join[fluxRules, paramRules];
  
  (* 3. ЗАЩИТА ОТ ВЫЧИСЛЕНИЙ (Изоляция в Hold)
     Оборачиваем выражения в Hold, чтобы Part ( [[idx]] ) не пытался вычислиться *)
  heldGrad = With[{g = gradSym}, Hold[g]] /. allRules;
  heldHess = With[{h = hessSym}, Hold[h]] /. allRules;
  
  (* 4. ГЕНЕРАЦИЯ COMPILE
     Трансформируем Hold-контейнер напрямую в вызов Compile.
     Это самый чистый способ передать готовое AST в компилятор. *)
  fastGrad = heldGrad /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}}, 
    body, 
    CompilationTarget -> "C", 
    RuntimeOptions -> "Speed",
    CompilationOptions -> {
        "ExpressionOptimization" -> True,
        "InlineExternalDefinitions" -> True
    }
  ];
  
  fastHess = heldHess /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}}, 
    body, 
    CompilationTarget -> "C", 
    RuntimeOptions -> "Speed",
    CompilationOptions -> {
        "ExpressionOptimization" -> True,
        "InlineExternalDefinitions" -> True
    }
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

End[];
EndPackage[];