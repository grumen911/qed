# Master Plan: QED Architecture Refactoring (Lazy Evaluation & Optimization)

Этот документ описывает дорожную карту перехода от монолитной архитектуры вычислений к графу зависимостей (Dependency Graph) с ленивыми вычислениями (Lazy Evaluation).

**Главные цели:**
1.  **Ускорение `FluxSweep` и `Plot`:** Исключить вычисление диагонализации там, где она не нужна (например, при расчете S-Matrix или классических токов).
2.  **JIT-совместимость:** Обеспечить мгновенный расчет произвольных точек рабочей точки для адаптивного сэмплирования в функции `Plot`.
3.  **Инкапсуляция:** Убрать прямой доступ функций `Numeric.wl` к внутренним структурам кэша `Model.wl`.

---

## Фаза 1: Декомпозиция (Создание `Calculators.wl`)

**Цель:** Выделить чистую логику вычислений из монолита `ComputeNumericalHarmonicPerturbation` в атомарные функции без побочных эффектов.

**Новый файл:** `src/Numeric/Calculators.wl`

- [ ] **1.1. Статический блок (Static Calculators)**
    - `CalcCompiledPotential[analytical]` $\to$ `CompiledFunction`.
        - *JIT Optimization:* Компилирует символьное выражение потенциала $U(\vec{\phi}, \Phi_{ext})$ в машинный код (C/LLVM).
        - *Цель:* Ускорение `FindRoot` в 10–100 раз за счет отказа от интерпретатора при вычислении $U$ и $\nabla U$.
    - `CalcDerivatives[analytical, params]` $\to$ `{Gradient, Hessian}`.
        - *Особенность:* Символьные производные, кэшируются "навечно".
    - `CalcStaticMatrices[analytical, params]` $\to$ `{C_num, InvC_num}`.
        - *Оптимизация:* Обращение матрицы емкости выносится сюда.

- [ ] **1.2. Классический блок (Equilibrium Calculator)**
    - `CalcEquilibrium[derivatives, phiExt, initialGuess]` $\to$ `{PhiMin, EffInd}`.
        - *Логика:* Обертка над `FindPotentialMinimumContinuation`. Должна принимать "подсказку" (`initialGuess`) из кэша для ускорения поиска.

- [ ] **1.3. Динамический блок (Dynamic Calculators)**
    - `CalcDynamicMatrices[analytical, phiMin]` $\to$ `{LInv_num, Operators_num}`.
    - `CalcDiagonalization[InvC, LInv]` $\to$ `{Frequencies, EigenVectors}`.
        - *Важно:* Это самая ресурсоемкая функция. Она должна вызываться **только** если запрошен Спектр, $T_1$ или Волновая функция.

- [ ] **1.4. Блок Рассеяния (Scattering Calculator)**
    - `CalcSMatrix[symS, effInd, freqs]` $\to$ `SMatrix`.
        - *Важно:* Реализовать "Быстрый путь" (Fast Path), использующий только `effInd`, минуя диагонализацию.

---

## Фаза 2: Движок Зависимостей (Refactoring `Model.wl`)

**Цель:** Научить `Model.wl` управлять состоянием кэша и вызывать калькуляторы по требованию.

- [ ] **2.1. Реестр Зависимостей (Dependency Registry)**
    - Создать структуру (`Association`), описывающую граф зависимостей.
    - Пример: `"SMatrix" -> {"EffectiveInductances", "StaticRules"}`.

- [ ] **2.2. Умный `GetNumericalQuantity`**
    - Переписать функцию так, чтобы она рекурсивно проверяла валидность ключей.
    - Логика: `Если (Cache[key] === Missing) -> Вызвать Calculator -> Записать в Cache`.

- [ ] **2.3. Механизм инвалидации (Dirty Flags)**
    - Вместо глобального флага `IsDirty`, внедрить точечный сброс.
    - При изменении `PhiExt` сбрасываем: `PhiMin`, `EffInd`, `LInv`, `Diag`, `SMatrix`.
    - Оставляем нетронутыми: `DerivCache`, `InvC`.

---

## Фаза 3: Рефакторинг Потребителей (`Numeric.wl`)

**Цель:** Убрать "Красную зону" (прямой доступ к кэшу) и перевести Sweep на ленивые рельсы.

- [ ] **3.1. Переписывание `GenerateFluxSweep`**
    - Убрать вызов `UpdateModelWithRules` внутри замыкания.
    - **Новая логика замыкания:**
        1. Создать копию модели (Lightweight Model).
        2. Обновить в ней значение `PhiExt`.
        3. Сбросить флаги зависимых узлов (`PhiMin` и ниже).
        4. Передать эту "ленивую" модель в `analysisFunc`.
    - **Результат:** `Plot[sweepFunc[x]]` сам вызывает `GetNumericalQuantity`, который сам решает, нужно ли считать диагонализацию.

- [ ] **3.2. Очистка `PlasmonFrequenciesVsFlux`**
    - Полностью перевести на использование API `GetNumericalQuantity`.

---

## Фаза 4: Оптимизация Классики (`FindPotentialMinimum`)

**Цель:** Сделать поиск рабочей точки мгновенным для `Plot` (Random Access).

- [ ] **4.1. Внедрение Smart Path Caching**
    - Внутри замыкания `GenerateFluxSweep` создать локальный кэш `{PhiExt -> PhiMin}`.
    - При запросе точки $\Phi_{target}$:
        1. Искать ближайшую точку $\Phi_{prev}$ в истории.
        2. Использовать $\phi_{min}(\Phi_{prev})$ как стартовое приближение для метода Ньютона.

- [ ] **4.2. Предиктор (Implicit Function Theorem) — *Альтернативная стратегия***
    - Реализовать вычисление $d\phi_{min}/d\Phi_{ext}$ через обратный Гессиан.
    - Использовать линейную экстраполяцию для старта поиска: $\phi_{guess} \approx \phi_{prev} + \phi' \cdot \Delta \Phi$.

---

## Фаза 5: Версионность и GUI (Advanced)

**Цель:** Мгновенный отклик GUI при возврате слайдера назад.

- [ ] **5.1. Хеширование Состояния**
    - Реализовать функцию `GetStateHash[model]`, зависящую от `Primary` параметров.
    - Превратить кэш модели в `VersionedCache[hash] -> Results`.

- [ ] **5.2. Интеграция в `Interactive.wl`**
    - При движении слайдера `GetNumericalQuantity` проверяет, было ли такое состояние (hash) вычислено ранее, прежде чем запускать пересчет.

---

# Справочные материалы (Архитектурные Графы)

## 1. Граф Вычислительной Логики (Runtime Logic)
*Показывает поток данных: какие величины нужны для вычисления других. Основа для `Calculators.wl`.*

```mermaid
graph TD
    %% --- ВХОДНЫЕ СОСТОЯНИЯ ---
    subgraph State [State Variables]
        StaticRules("Static Rules<br/>(C, L, EJ)")
        PhiExt("External Flux<br/>(Phi_ext)")
    end

    %% --- 1. СТАТИЧЕСКИЙ БЛОК (Pre-calc) ---
    subgraph Block_Static [Static Calculators]
        Derivatives("Derivatives of U")
        NumC("Matrix C_num")
        InvC("Inverse C (C^-1)")
        
        StaticRules --> Derivatives
        StaticRules --> NumC
        NumC --> InvC
    end

    %% --- 2. БЛОК РАБОЧЕЙ ТОЧКИ (Equilibrium) ---
    subgraph Block_Equilibrium [Equilibrium Calculator]
        PhiMin("Equilibrium Fluxes<br/>(Phi_min)")
        EffInd("Effective Inductances<br/>(L_eff)")
        
        Derivatives --> PhiMin
        PhiExt --> PhiMin
        PhiMin --> EffInd
        StaticRules --> EffInd
    end

    %% --- 3. ДИНАМИЧЕСКИЕ МАТРИЦЫ ---
    subgraph Block_DynamicMats [Dynamic Matrices]
        NumLInv("Matrix L^-1_num")
        NumOps("Operators Num")
        
        StaticRules --> NumLInv
        PhiMin --> NumLInv
        PhiMin --> NumOps
    end

    %% --- 4. СПЕКТРАЛЬНАЯ ЗАДАЧА (Heavy) ---
    subgraph Block_Eigen [Diagonalization Calculator]
        DiagData("EigenSystem<br/>(Freqs, Vectors)")
        
        InvC --> DiagData
        NumLInv --> DiagData
    end

    %% --- ПОТРЕБИТЕЛИ (Outputs) ---
    subgraph Outputs [Consumers]
        SMatrix("S-Matrix (Fast Path)")
        EffInd --> SMatrix
        StaticRules --> SMatrix
        
        Spectrum("Spectrum (Slow Path)")
        DiagData --> Spectrum
    end
```

## 2. Граф Доступа к Данным (Target Architecture)
*Показывает целевое состояние: все функции `Numeric.wl` используют только публичный API.*

```mermaid
graph LR
    %% --- СТИЛИЗАЦИЯ ---
    classDef redZone fill:none,stroke:#d32f2f,stroke-width:3px,color:black;
    classDef greenZone fill:none,stroke:#388e3c,stroke-width:2px,color:black;
    classDef dataNode fill:#eceff1,stroke:#546e7a,stroke-width:1px,color:black;
    classDef modelAPI fill:#e8f5e9,stroke:#2e7d32,stroke-width:4px,color:black;

    %% --- MODEL.WL ---
    subgraph Model_WL [Model.wl]
        direction TB
        API[[GetNumericalQuantity]]:::modelAPI
        
        subgraph Internals [Private Cache]
            DerivCache[(Derivatives)]:::dataNode
            DiagCache[(Diagonalization)]:::dataNode
        end
    end

    %% --- NUMERIC.WL ---
    subgraph Numeric_WL [Numeric.wl]
        direction TB
        
        %% SWEEP ФУНКЦИИ
        subgraph Sweep_Logic [Flux Sweep Logic]
            GenFluxSweep[GenerateFluxSweep]
            PlasmonVsFlux[PlasmonFrequenciesVsFlux]
        end
        class Sweep_Logic greenZone
        
        %% CONSUMERS
        subgraph Consumers [Calculators]
            CalcRates[CalculateFermiRates]
        end
        class Consumers greenZone
    end

    %% --- СВЯЗИ ---
    %% Все стрелки должны быть ЗЕЛЕНЫМИ (через API)
    
    GenFluxSweep -- "Sets PhiExt & Dirty Flags" --> API
    PlasmonVsFlux -- "GetNumQ['Spectrum']" --> API
    CalcRates -- "GetNumQ['Operators']" --> API
    
    %% Внутренняя кухня модели (скрыта)
    API -.-> DerivCache
    API -.-> DiagCache
```

## 3. Граф Оптимизации Sweep (Functional JIT Architecture)
*Этот граф показывает структуру Чистой Функции, которую возвращает `GenerateFluxSweep`.*
* **Серый блок (Captured Context)**: Данные, которые вычисляются 1 раз и "живут" внутри замыкания.
* **Зеленый блок (Execution Flow)**: То, что происходит при каждом вызове функции (например, внутри `Plot`).

```mermaid
graph TD
    %% --- СТИЛИЗАЦИЯ ---
    classDef factory fill:#e3f2fd,stroke:#1565c0,stroke-width:2px;
    classDef closure fill:#fff8e1,stroke:#ff8f00,stroke-width:2px,stroke-dasharray: 5 5;
    classDef exec fill:#e8f5e9,stroke:#2e7d32,stroke-width:2px;
    classDef input fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px;
    classDef waste fill:#ffcdd2,stroke:#c62828,stroke-width:2px;

    %% --- 1. ФАЗА ИНИЦИАЛИЗАЦИИ (FACTORY) ---
    CallGen["Client calls:<br/>GenerateFluxSweep[model, analysisFunc]"]:::factory
    
    %% --- 2. ЗАМЫКАНИЕ (ВОЗВРАЩАЕМАЯ ФУНКЦИЯ) ---
    %% ИСПРАВЛЕНО: Убраны кавычки внутри [ ] для подграфа
    subgraph Closure_Scope [Returned Pure Function: Function of phi]
        direction TB
        
        %% A. ЗАХВАЧЕННЫЙ КОНТЕКСТ (CAPTURED)
        %% ИСПРАВЛЕНО: Убраны кавычки для подграфа
        subgraph Context [Captured Context - Private State]
            StaticData["Static Data:<br/>Derivatives, InvC"]
            BaseModel["Lightweight Model Copy"]
            PathCache["Mutable Path Cache:<br/>History of phi -> phi_min"]
        end

        %% B. АРГУМЕНТ (ITERATOR)
        Arg(("Input: phi")):::input
        
        %% C. ПОТОК ВЫПОЛНЕНИЯ (EXECUTION)
        %% ИСПРАВЛЕНО: Убраны кавычки для подграфа
        subgraph Execution [Execution Logic - Per Call]
            %% 1. Predict
            Predict["1. Smart Predict:<br/>Get InitialGuess from PathCache"]
            
            %% 2. Solve (Fast)
            Solve["2. Solve Equilibrium:<br/>CalcEquilibrium[phi, guess]"]
            
            %% 3. Update State
            Update["3. Update PathCache &<br/>Set Dirty Flags in Model"]
            
            %% 4. Run User Func
            RunUser["4. Execute:<br/>analysisFunc[lazyModel]"]
        end
        
        %% СВЯЗИ ВНУТРИ ЗАМЫКАНИЯ
        Arg --> Predict
        PathCache -.-> Predict
        StaticData -.-> Solve
        Predict --> Solve
        Solve --> Update
        Update -.-> PathCache
        BaseModel -.-> RunUser
        Update --> RunUser
    end

    %% --- 3. ЛЕНИВЫЙ ЗАПРОС (JIT) ---
    %% ИСПРАВЛЕНО: Убраны кавычки для подграфа
    subgraph Lazy_Resolution [Lazy Evaluation Graph]
        Req{analysisFunc requests...}
        
        %% Путь 1: Быстрый (S-Matrix)
        Req -- "S-Matrix" --> GetEff[Get EffInd]
        Solve -.-> GetEff
        
        %% Путь 2: Медленный (Игнорируется если не нужен)
        Req -- "Spectrum" --> CalcDiag[Calc Diagonalization]:::waste
    end

    %% --- ВНЕШНИЕ СВЯЗИ ---
    CallGen --> Closure_Scope
    RunUser --> Req
    CalcDiag --> Result([Result])
    GetEff --> Result
    
    %% ДЕМОНСТРАЦИЯ PLOT
    Plot[Plot calls function at phi=0.35] -.-> Arg
```
## 4. Граф дуальной JIT ахитектуры 

```mermaid
graph TD
    %% --- ВНЕШНИЙ СЛОЙ: ARCHITECTURAL JIT ---
    subgraph Outer_JIT ["Level 1: Architectural JIT (Sweep / Lazy)"]
        direction TB
        PlotNode["Plot requests point x=0.35"]
        Closure["Lazy Closure (The Manager)"]
        
        PlotNode -- "Order: Calc point 0.35" --> Closure
    end

    %% --- ВНУТРЕННИЙ СЛОЙ: COMPUTATIONAL JIT ---
    subgraph Inner_JIT ["Level 2: Computational JIT (Compiled Math)"]
        direction TB
        Logic["FindPotentialMinimum Logic"]
        
        %% Сравнение двух подходов
        subgraph Approaches ["Optimization Strategy"]
            Slow["Interpreter (Current):<br/>Symbolic Engine<br/>Slow Tree Traversal"]
            
            Fast["Compiled Code (Target):<br/>Machine Code (C/LLVM)<br/>Native CPU Speed"]
        end
        
        Closure -- "Run Minimization" --> Logic
        Logic -- "Evaluates U(phi)" --> Fast
    end

    %% --- ИТОГ ---
    Result([Super Fast Result])
    Fast --> Result
```