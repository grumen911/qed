# CAD for superconducting circuits — architecture notes (draft)

Goal: build a standalone, highly extensible CAD-like application for modeling and simulation of superconducting circuits.

## Top-level requirements

- Standalone desktop app (cross-platform preferred).
- Highly extensible: new circuit elements, analyses/solvers, and views should be addable without modifying the core.
- Clear separation: GUI (Qt) → model/state → computational core.
- Reproducibility: deterministic runs, versioned inputs, and exportable results.

## Proposed architecture

### Layers

1. **GUI layer (Qt)**
- Responsible for windows, docking layout, editors (schematic + parameter tables), and interactive controls.
- Does not implement physics/solvers; it only reads/writes the application state.
- Supports state initialization via auto-loaded presets/templates upon startup.
- Automatically handles formatting, parameter grouping, and physical dimension/unit display in editors without bleeding into the core compute logic.

1. **Model / state layer (Document model)**
- Single source of truth: circuit graph, component parameters, analysis configuration, computed results, caches.
- Emits change notifications (signals/slots or observer pattern) so the UI and compute scheduler react to edits.

1. **Computational core (engine)**
- Pure “business logic”: equation assembly, symbolic/analytic steps where relevant, numeric solvers, postprocessing.
- No Qt dependency; designed to be unit-testable and callable from different front-ends.
- Strict module isolation: Enforces acyclic dependencies between physical models and mathematical solvers (e.g., isolating system parameter derivation from quantum scattering calculations) to prevent cyclic import issues.
- Caching strategy: Internal caches (for numerical quantities and rules) are strictly versioned with the Document state to prevent stale data reuse.

### Data flow (reactive but controlled)

- User edits a parameter (slider/table) → GUI writes to Document.
- Document emits `changed(...)` → scheduler decides what becomes invalid.
- Scheduler triggers recomputation (possibly async) → results are written back to Document.
- Views subscribe to Document signals and update plots/tables.
- **Dependency Graph (DAG) for evaluation:** The scheduler builds a directed acyclic graph of dependencies rather than using naive lazy evaluation. This allows explicitly separating classical (e.g., effective inductances, flux sweeps) and quantum (e.g., scattering, harmonic perturbations) calculation steps, ensuring they are only recomputed when their specific upstream dependencies change.

This mimics WL-like dynamic updates, but with explicit invalidation/caching rules.

## Extensibility via plugins

### Plugin goals

- Add new functionality by dropping a plugin binary into a folder.
- Core app discovers and loads plugins at startup (optionally later: hot reload).

### Recommended plugin categories

- **ElementPlugin**
  - Defines a new circuit element type.
  - Provides: parameter schema + default values; stamping/assembly rules; optional icon/graphics.

- **AnalysisPlugin**
  - Defines a new analysis (e.g., spectrum, mode-finding, sweeps, optimization, noise models).
  - Provides: configuration schema; compute entry point; output schema.

- **ViewPlugin**
  - Adds a dockable panel (plot viewer, sweep explorer, report generator, etc.).
  - Reads results from Document and renders them.
  - *Advanced visualization:* Supports publication-quality plot rendering with LaTeX-formatted labels.
  - *Inspection views:* Includes debugging views (e.g., a "file manager" style hierarchical inspector for raw internal caches, parameters, and numeric quantities).

### Qt plugin mechanism (C++/Qt)

- Use Qt’s plugin system (e.g., `QPluginLoader`) to load shared libraries at runtime.
- Each plugin implements a stable interface (pure abstract C++ interface + Qt metadata).

## Suggested project layout

- `app/` — Qt GUI executable, windowing, docking, actions, commands.
- `core/` — Document model, change tracking, serialization, plugin interfaces.
- `engine/` — numerical/symbolic routines, solvers, assembly.
- `plugins/`
  - `elements/` — element plugins
  - `analyses/` — analysis plugins
  - `views/` — view plugins
- `tests/` — unit tests for engine and core (no GUI).
- `examples/` — small circuits + configs used as regression tests.

## Key technical decisions (to decide early)

- Language split:
  - Option A (maximum robustness): C++/Qt for GUI + plugins; engine in C++/Fortran; optional Python scripting.
  - Option B (fast start): PySide6 GUI + engine in Python/C++; later migrate plugins to C++.
- Plugin loading policy: startup-only vs hot-reload.
- Serialization: choose a stable format for circuits + runs (e.g., JSON/YAML + versioning).
- Compute scheduling: synchronous for small tasks; async worker threads for sweeps/diagonalization.

## Minimal viable prototype (MVP)

1. Document model + serialization.
2. Basic schematic editor (or graph-based editor) + parameter panel.
3. One element plugin set (e.g., L, C, JJ) with stamping rules.
4. One analysis plugin (e.g., Hamiltonian build + eigenvalues for a small circuit).
5. One view plugin (spectrum plot + parameter sweep plot).

## Quality and reproducibility

- Deterministic runs (fixed seeds where relevant).
- Version tag in output: core/engine/plugin versions and input hash.
- Regression tests based on small circuits with known results.
- Standardized logging and error reporting (no silent failures or raw `print` statements; strictly routed through a central messaging/logging system).

## Related existing tools (ecosystem survey)

This project does not start from zero: there are already open-source and commercial tools around superconducting-qubit chip/layout design and analysis. A practical strategy is to interoperate with them (import/export and workflow integration) rather than re-implement everything.

### Open-source CAD / EDA / layout-oriented

- **KLayout** — 2D layout viewer/editor widely used in chip design flows.
  - License: free and open-source (KLayout sources are distributed under GNU GPL; verify exact version terms for your distribution/repository).
  - Useful for: GDS/OASIS-centric layout editing, DRC scripting, and being a host application for Python/Ruby automation.

- **Qiskit Metal (Quantum Metal)** — Python-based framework for quantum hardware (superconducting) device design & analysis.
  - License: Apache License 2.0.
  - Useful for: parametric design generation, integration with EM simulation workflows, and building reproducible design scripts.

- **KQCircuits** — open-source EDA framework built on top of KLayout (Python) for designing superconducting quantum processors.
  - License: see the project repository (verify before redistribution/integration).
  - Useful for: defining reusable design elements and producing fabrication-ready layout patterns.

- **DeviceLayout.jl** — CAD tooling for quantum integrated circuits (Julia ecosystem).
  - License: see project repository (verify before integration).
  - Useful for: schematic-driven / parametric design at scale, plus possible integration with open-source EM solvers.

### Commercial / platform tools

- **QTCAD (Nanoacademic)** — commercial toolchain for quantum technology device simulation; can be used in workflows that integrate with open-source layout generators.
  - License: commercial.
  - Useful for: integrated simulation workflows (FEM/EM/device simulation), depending on your lab/company tooling.

### Notes on licensing strategy for this project

- Prefer keeping the computational core license-friendly (permissive, no hard dependence on GPL components).
- Treat external EDA/CAD tools as optional integrations via import/export, file formats, or subprocess calls.
- Always verify the exact license of the specific version you depend on (especially when distributing binaries or bundling plugins).
