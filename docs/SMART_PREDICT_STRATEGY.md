# Smart Predict Architecture (Phase 4)

## Концепция: First-Order Predictor для JIT-конвейера

В данном документе описана стратегия перехода от "Предсказателя нулевого порядка" к "Предсказателю первого порядка" (Smart Predict) при поиске равновесных магнитных потоков в конвейере `GenerateSweepPipeline`.

### 1. Текущая реализация (Zero-Order Predictor)
На данный момент JIT-конвейер осуществляет шаги по внешнему магнитному потоку (от Φ1 к Φ2). В качестве стартовой догадки (`currentGuess`) для метода Ньютона на шаге Φ2 передается точное положение минимума с предыдущего шага Φ1. 

Проблема:
Поскольку положение дна потенциальной ямы смещается при изменении внешнего потока, старая догадка оказывается неточной. Из-за этого локальному решателю `FindRoot` требуется 3-5 итераций для сходимости к новому минимуму, что замедляет генерацию плотных графиков.

### 2. Математическое обоснование (Smart Predict)
Для вычисления точного смещения минимума мы можем использовать Теорему о неявной функции.

Условие экстремума потенциала:
g(φ_min, Φ_ext) = 0
Где g — вектор градиента потенциальной энергии.

Продифференцируем это уравнение по внешнему потоку Φ_ext:
H * (dφ_min / dΦ_ext) + (∂g / ∂Φ_ext) = 0
Где H — матрица Гессе (Гессиан).

Отсюда вектор скорости смещения минимума v:
v = dφ_min / dΦ_ext = - H^(-1) * (∂g / ∂Φ_ext)

Имея вектор скорости v, мы можем сделать "Умную догадку" (шаг Эйлера) для нового положения потока:
smartGuess = currentGuess + stepSize * v

### 3. План реализации в коде

Шаг 1: Обновление JIT-компилятора (Calculators.wl)
В функции `CalcCompiledEngines` необходимо добавить вычисление и компиляцию вектора смешанных производных (градиента по фазам узлов, продифференцированного по внешнему потоку).

Псевдокод изменений в CalcCompiledEngines:
  // Существующий код
  gradSym = D[potRescaled, {fluxSymbols}];
  hessSym = D[gradSym, {fluxSymbols}];
  
  // ДОБАВИТЬ: Смешанная производная по внешнему потоку (p[phiExtIndex])
  mixedDerivSym = D[gradSym, paramSymbols[[phiExtIndex]]];
  
  // Скомпилировать mixedDerivSym в fastMixedDeriv и вернуть в списке engines

Шаг 2: Интеграция предиктора в конвейер (Numeric.wl)
В функции `GenerateSweepPipeline` внутри цикла `Do` перед вызовом `CalcEquilibrium` необходимо рассчитать вектор скорости и применить его к догадке.

Псевдокод изменений в GenerateSweepPipeline:
  Do[
    currentPhi = nearestPhi + i * stepSize;
    currentParamVector[[phiExtIndex]] = currentPhi * QED`$Phi0Value;
    
    // ДОБАВИТЬ: Smart Predict (Шаг Эйлера)
    currentHessian = fastHess[currentGuess, currentParamVector];
    currentMixed = fastMixedDeriv[currentGuess, currentParamVector];
    
    // Решаем систему: H * v = - mixed
    velocity = LinearSolve[currentHessian, -currentMixed]; 
    smartGuess = currentGuess + stepSize * velocity;
    
    // Передаем smartGuess вместо currentGuess
    currentGuess = QED`Numeric`Calculators`CalcEquilibrium[
      engines, smartGuess, currentParamVector
    ];
    
    pathHistory[currentPhi] = currentGuess;
  , {i, 1, steps}]

### 4. Ожидаемый результат
* Идеальная стартовая догадка: `smartGuess` будет попадать практически в точное новое дно потенциала.
* Ускорение сходимости: Метод Ньютона внутри `CalcEquilibrium` будет сходиться за 1 итерацию (или 0, если отключить `FindRoot` для очень мелких шагов).
* Общее ускорение генерации карт S-параметров и спектроскопии в 2-3 раза при использовании плотных сеток.