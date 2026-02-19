# Master Plan: QED Architecture Refactoring (Lazy Evaluation & JIT)

Этот документ описывает дорожную карту перехода архитектуры пакета `qed` от монолитных вычислений к оптимизированному графу зависимостей.

**Главные цели:**
1.  **Производительность:** Внедрение JIT-компиляции (Procedural C-Style) для поиска классического минимума (ускорение 20-50x).
2.  **Ленивость:** Исключение тяжелых квантовых расчетов (диагонализации) при свипах S-матрицы.
3.  **Стабильность:** Устранение утечек памяти в замыканиях и безопасное управление кэшем.

---

## Фаза 0: Подготовка Данных (Data Marshalling)

**Цель:** Подготовить инфраструктуру для быстрой передачи параметров в скомпилированные функции, минуя медленные правила замен (`Rules`).

- [x] **0.1. Канонизация Параметров**
    - Реализовать жесткую сортировку параметров модели (алфавитную).
    - Создать функцию `GetParameterVector[model]` $\to$ `List of Reals` (Packed Array).
    - *Обоснование:* Скомпилированные функции (`Compile`) требуют плоский вектор чисел на входе. Глобальные переменные или `Rules` убивают производительность.

---

## Фаза 1: Декомпозиция (Calculators.wl)

**Цель:** Выделить чистую логику вычислений в атомарные функции.

**Новый файл:** `src/Numeric/Calculators.wl`

- [x] **1.1. Статический блок (JIT Core)**
    - `CalcCompiledEngines[analytical]` $\to$ `{FastGrad, FastHess}`.
        - *Техника:* Процедурная компиляция (`Do` loops внутри `Compile`). Избегать `Evaluate` сложных выражений.
        - *Вход:* Вектор координат $\vec{\phi}$ + Вектор параметров $\vec{p}$.
        - *Выход:* Градиент (вектор) и Гессиан (матрица) как `CompiledFunction` (Target: C).
    - `CalcStaticMatrices[analytical, params]` $\to$ `{C_num, InvC_num}`.
        - *Оптимизация:* Обращение матрицы емкости ($C^{-1}$) выполняется здесь один раз.

- [ ] **1.2. Классический блок (Equilibrium Calculator)**
    - `CalcEquilibrium[compiledEngines, phiExt, guess, paramVector]` $\to$ `{PhiMin, EffInd}`.
        - *Логика:* Вызывает `FindRoot` с явным указанием `Jacobian -> FastHess`.
        - *Результат:* Мгновенная сходимость метода Ньютона.

- [ ] **1.3. Динамический блок (Dynamic Calculators)**
    - `CalcSystemMatrices[analytical, phiMin]` $\to$ `{LInv_num, Operators_num}`.
        - *Укрупнение:* Считаем матрицы и операторы вместе.
    - `CalcEigenSystem[InvC, LInv]` $\to$ `{Frequencies, EigenVectors}`.
        - *Важно:* Самая тяжелая функция. Вызывается **строго** по запросу (Lazy).

- [ ] **1.4. Блок Рассеяния (Numeric Scattering)**
    - *Проблема:* Текущий `Scattering.wl` работает символьно.
    - *Решение:* Реализовать `CalcSMatrixNumeric[InvC, EffInd, Freqs]`.
    - *Логика:* Принимать численные матрицы (Packed Arrays) и решать `LinearSolve` напрямую.

---

## Фаза 2: Движок Зависимостей (Model.wl)

**Цель:** Научить `Model.wl` управлять состоянием кэша.

- [ ] **2.1. Реестр Зависимостей**
    - Описать граф связей (например, `"Spectrum"` $\to$ `"SystemMatrices"`).

- [ ] **2.2. Умный `GetNumericalQuantity`**
    - Реализовать рекурсивный опрос: `Check Cache -> If Missing -> Call Calculator -> Set Cache`.

- [ ] **2.3. Стратегия Инвалидации (Hash-based Versioning)**
    - Ключ кэша = `Hash[{PhiExt, ParameterVector}]`.
    - *Плюс:* Автоматически решает проблему "грязного кэша" и позволяет реализовать мгновенный Undo/Redo в GUI.

---

### Стратегии организации кэша для Свипа (задел на Фазу 3/4)

Для реализации "Умного предсказания" (Smart Predict) начальной точки в методе Continuation мы используем кэширование истории вычислений `{PhiExt -> PhiMin}`. 

* **Встроенный `Nearest`:** В Wolfram Language есть супероптимизированная функция `Nearest[keys -> values]`, которая под капотом строит KD-дерево.
    * **Плюс:** Поиск ближайшего соседа (стартовой точки для метода Ньютона) занимает наносекунды. Это идеально ложится на адаптивный алгоритм шагов встроенного `Plot`.
    * **Минус:** При добавлении каждой новой вычисленной точки KD-дерево нужно перестраивать заново. 
    * **Вердикт:** Для типичного графика, где количество точек $N < 1000$, суммарная перестройка дерева занимает микросекунды. Сложность алгоритма не станет узким местом системы, так как основное время всё равно уходит на JIT-вычисления в `FindRoot`. Это самый надежный и чистый "коробочный" подход.

---

## Фаза 3: Рефакторинг Потребителей (Numeric.wl)

**Цель:** Перевести Sweep на ленивые рельсы.

- [ ] **3.1. Фабрика Sweep (`GenerateFluxSweep`)**
    - **Stripped Closure:** Перед созданием замыкания создавать "Скелет Модели" (`Lightweight Model`) — копию без тяжелых данных.
    - **Lazy Execution:** Внутри замыкания обновлять `PhiExt` и вызывать `analysisFunc`.

- [ ] **3.2. Очистка API**
    - Перевести все внешние функции на использование `GetNumericalQuantity`.

---

## Фаза 4: Оптимизация Классики (Smart Pathing)

**Цель:** Ускорение свипов за счет предсказания начальной точки.

- [ ] **4.1. Smart Path Caching**
    - Внутри замыкания свипа хранить локальную историю `{PhiExt -> PhiMin}`.
    - Использовать результат ближайшего соседа как `InitialGuess` для `FindRoot`.
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