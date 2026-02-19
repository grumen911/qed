BeginPackage["QED`Numeric`Calculators`"];


CalcCompiledEngines::usage = "CalcCompiledEngines[analytical, fluxSymbols, paramSymbols] \
generates JIT-compiled C-functions {FastGrad, FastHess} for root finding.";

CalcStaticMatrices::usage = "CalcStaticMatrices[analytical, rules] computes the static \
numerical capacitance matrix and its inverse. Returns {C_num, InvC_num}.";

CalcEquilibrium::usage = "CalcEquilibrium[compiledEngines, guess, paramVector] performs \
a fast local Newton search for equilibrium flux using JIT engines.";

CalcSystemMatrices::usage = "CalcSystemMatrices[fastLInv, phiMin, paramVector] computes \
the numeric inverse inductance matrix using JIT.";

CalcEigenSystem::usage = "CalcEigenSystem[invCNum, invLNum] computes normal mode frequencies \
(omega) and eigenvectors from C^-1 . L^-1.";

CalcSMatrixNumeric::usage = "CalcSMatrixNumeric[omega, cNum, invLNum, portIndices, z0] \
calculates the numerical S-matrix at a given angular frequency using floating ground expansion and Schur complement.";

Begin["`Private`"];


CalcCompiledEngines[analytical_Association, fluxSymbols_List, paramSymbols_List] := Module[
  {
    phi0, energyScale, constantRules, 
    potSym, potRescaled, gradSym, hessSym, 
    minSymbols, linvSym, minRules, allRulesLInv, heldLInv, fastLInv,
    fluxRules, paramRules, allRules,
    heldGrad, heldHess, fastGrad, fastHess
  },
  
  phi0 = QED`$Phi0Value;
  energyScale = QED`$hbarValue * 2 * Pi * 10^9; 
  
  constantRules = {
    QED`$Phi0 -> phi0,
    QED`$hbar -> QED`$hbarValue,
    QED`$e   -> QED`$eValue
  };
  
  (* --- БЛОК 1: РАВНОВЕСИЕ (Градиент и Гессиан) --- *)
  potSym = analytical["Potential"] /. constantRules;
  potRescaled = (potSym / energyScale) /. 
    Table[fluxSymbols[[i]] -> fluxSymbols[[i]] * phi0, {i, Length[fluxSymbols]}];
  
  gradSym = D[potRescaled, {fluxSymbols}];
  hessSym = D[gradSym, {fluxSymbols}];
  
  fluxRules = Table[With[{idx = i}, fluxSymbols[[idx]] :> phi[[idx]]], {i, Length[fluxSymbols]}];
  paramRules = Table[With[{idx = i}, paramSymbols[[idx]] :> p[[idx]]], {i, Length[paramSymbols]}];
  allRules = Join[fluxRules, paramRules];
  
  heldGrad = With[{g = gradSym}, Hold[g]] /. allRules;
  heldHess = With[{h = hessSym}, Hold[h]] /. allRules;
  
  fastGrad = heldGrad /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}}, body, CompilationTarget -> "C", RuntimeOptions -> "Speed", CompilationOptions -> {"ExpressionOptimization" -> True, "InlineExternalDefinitions" -> True}];
  fastHess = heldHess /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}}, body, CompilationTarget -> "C", RuntimeOptions -> "Speed", CompilationOptions -> {"ExpressionOptimization" -> True, "InlineExternalDefinitions" -> True}];
  
  (* --- БЛОК 2: ДИНАМИЧЕСКИЕ МАТРИЦЫ (L^-1) --- *)
  (* Символы минимума: Subscript[Phi, "min", i] *)
  minSymbols = fluxSymbols /. Subscript[s_, i_] :> Subscript[s, "min", i];
  linvSym = analytical["InductanceMatrix"] /. constantRules;
  
  minRules = Table[With[{idx = i}, minSymbols[[idx]] :> phi[[idx]]], {i, Length[minSymbols]}];
  allRulesLInv = Join[minRules, paramRules];
  heldLInv = With[{m = linvSym}, Hold[m]] /. allRulesLInv;
  
  fastLInv = heldLInv /. Hold[body_] :> Compile[{{phi, _Real, 1}, {p, _Real, 1}}, body, CompilationTarget -> "C", RuntimeOptions -> "Speed", CompilationOptions -> {"ExpressionOptimization" -> True, "InlineExternalDefinitions" -> True}];
  
  (* Возвращаем тройку движков *)
  {fastGrad, fastHess, fastLInv}
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

CalcEquilibrium[{fastGrad_, fastHess_, ___}, guess_List, paramVector_?Developer`PackedArrayQ] := Module[
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

CalcSystemMatrices[fastLInv_CompiledFunction, phiMin_List, paramVector_?Developer`PackedArrayQ] := <|
  "InverseInductance" -> fastLInv[phiMin, paramVector]
|>;

CalcEigenSystem[invCNum_?MatrixQ, invLNum_?MatrixQ] := Module[
  {sysMat, vals, vecs, freqs, sortingIndices},
  
  sysMat = invCNum . invLNum;
  {vals, vecs} = Eigensystem[sysMat];
  
  (* Извлекаем частоты. ComplexExpand страхует от отрицательных собственных чисел (мнимая частота = седловая точка) *)
  freqs = Sqrt[ComplexExpand[vals]];
  
  (* Сортируем по возрастанию действительной части *)
  sortingIndices = Ordering[Re[freqs]];
  
  <|
    "Frequencies" -> freqs[[sortingIndices]],
    "EigenVectors" -> vecs[[sortingIndices]]
  |>
];

ExpandToFloatingMatrix[mat_?MatrixQ] := Module[
  {rowSums, colSums, totalSum},
  rowSums = -Total[mat, {2}]; (* Сумма по строкам *)
  colSums = -Total[mat, {1}]; (* Сумма по столбцам *)
  totalSum = -Total[rowSums]; (* Сумма всех элементов *)
  
  (* Собираем новую матрицу блоками *)
  ArrayFlatten[{{mat, Transpose[{rowSums}]}, {List[colSums], totalSum}}]
];

CalcSMatrixNumeric[omega_?NumericQ, cNum_?MatrixQ, invLNum_?MatrixQ, portIndices_List, z0_Real:50.0] := Module[
  {
    cFull, invLFull, yFull, 
    pIdx, iIdx, 
    yPP, yPI, yIP, yII, yReduced, 
    nPorts, id, sMat
  },
  
  (* 1. Восстанавливаем "плавающую землю" (GroundNode становится последним индексом N+1) *)
  cFull = ExpandToFloatingMatrix[cNum];
  invLFull = ExpandToFloatingMatrix[invLNum];
  
  (* 2. Собираем полную комплексную Y-матрицу схемы: Y = 1/(i w L) + i w C *)
  yFull = (1.0 / (I * omega)) * invLFull + (I * omega) * cFull;
  
  (* 3. Распределяем индексы на Порты (P) и Внутренние (I) *)
  pIdx = portIndices;
  iIdx = Complement[Range[Length[yFull]], pIdx];
  
  (* 4. Блочное разбиение и Шуровское исключение внутренних узлов *)
  yPP = yFull[[pIdx, pIdx]];
  
  If[Length[iIdx] > 0,
    yPI = yFull[[pIdx, iIdx]];
    yIP = yFull[[iIdx, pIdx]];
    yII = yFull[[iIdx, iIdx]];
    
    (* Быстрое исключение через LAPACK/MKL: yPI . (yII^-1 . yIP) *)
    yReduced = yPP - yPI . LinearSolve[yII, yIP];
  ,
    yReduced = yPP;
  ];
  
  (* 5. Преобразование Y -> S *)
  nPorts = Length[pIdx];
  id = IdentityMatrix[nPorts, WorkingPrecision -> MachinePrecision];
  
  (* Формула: S = (I - Z0*Y) . (I + Z0*Y)^-1 *)
  sMat = LinearSolve[id + z0 * yReduced, id - z0 * yReduced];
  
  Developer`ToPackedArray[sMat, Complex]
];

End[];
EndPackage[];