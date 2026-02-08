# QED Package: GUI Architecture & QubitDashboard

This document describes the internal architecture of the `QubitDashboard` and the `Interactive` module. It focuses on the event-driven update model designed to handle computationally intensive tasks (like $T_1$ relaxation or 3D potential landscapes) without freezing the user interface or triggering `$Aborted` timeouts.

## 1. Core Philosophy: Event-Driven vs. Reactive

Standard Wolfram Language `Manipulate` or `Dynamic` interfaces are **reactive**: any change in a variable immediately triggers a re-evaluation. While simple, this approach fails for heavy computations because `Dynamic` evaluations run on the **Preemptive Link**, which has a strict time limit (typically 5-6 seconds).

The **QED Dashboard** uses an **Event-Driven** architecture with explicit state management.

### Key Concepts
1.  **View (Dynamic)**: Purely passive. It only renders the current state stored in `plotCache`. It never initiates computation.
2.  **Controller (Button/Sliders)**: Initiates computation explicitly.
3.  **State (Cache)**: Acts as a buffer between calculation and rendering.

---

## 2. Thread Management: Solving `$Aborted`

To prevent timeouts during heavy calculations (e.g., finding equilibrium in a complex flux landscape), we utilize the **Main Link** via `Method -> "Queued"`.

### The Update Mechanism (`performUpdate`)

The update logic is encapsulated in a local function `performUpdate`:

```wolfram
performUpdate = Function[{},
    (* 1. Immediate Feedback *)
    plotCache[selectedPlotId] = "Computing...";
    FinishDynamic[]; (* FORCE UI update before computation starts *)
    
    (* 2. Heavy Computation *)
    Module[{res, updatedModel},
       {res, updatedModel} = ComputePlotData[...];
       plotCache[selectedPlotId] = res; (* Update State *)
    ]
];
```

* **`FinishDynamic[]`**: This is critical. It forces the FrontEnd to draw the "Computing..." spinner *before* the kernel gets busy. Without this, the UI would freeze showing the old plot.
* **Blocking Behavior**: While `performUpdate` runs, the UI is effectively "frozen" (watch cursor). In this scientific context, this is a **feature**, ensuring atomicity (the user cannot change parameters while the model is inconsistent).

---

## 3. Sliders & Controls Logic

The dashboard implements a "Lazy Update" pattern to distinguish between "Light" (fast) and "Heavy" (slow) plots.

### Plot Classification
Plots are registered via `RegisterPlot` with a type:
* **"Light"**: Instant calculation (e.g., Plasmon Spectrum).
* **"Heavy"**: Requires numerical optimization or matrix integration (e.g., Relaxation Time, Wavefunctions).

### Control Behavior

| Interaction | "Light" Plot (e.g., Spectrum) | "Heavy" Plot (e.g., T1) |
| :--- | :--- | :--- |
| **Slider Move** | **Immediate Update.** The slider calls `performUpdate` directly. The UI remains responsive because the calculation is sub-second. | **Mark as Stale.** The slider does *not* compute. It sets `plotCache[...] = Missing["Stale"]`. The View displays "Parameters changed. Press Update". |
| **"Update" Button** | Forces a re-calculation (redundant but safe). | **Queued Execution.** Calls `performUpdate` on the Main Link. No timeout limit. |

### Implementation Detail
```wolfram
(* Inside Slider callback *)
Function[{}, 
   If[isLightPlot,
      performUpdate[],                 (* Fast path *)
      plotCache[id] = Missing["Stale"] (* Lazy path *)
   ]
]
```

---

## 4. State Management (Caching)

The dashboard maintains two levels of state:

1.  **`currentModel`**: The source of truth. Contains the `IsDirty` flag.
    * When a slider moves, `IsDirty` becomes `True`.
    * `ComputePlotData` handles the "Warm Up" (re-calculating equilibrium if needed).

2.  **`plotCache`**: A transient Association storing the last rendered Graphics.
    * `Keys`: Plot IDs (e.g., "PlasmonSpectrum").
    * `Values`: `Graphics` object, `"Computing..."` string, or `Missing[...]`.

This separation ensures that `Dynamic` (the View) never attempts to call `ComputePlotData` directly, avoiding the "Preemptive Link" timeout trap.