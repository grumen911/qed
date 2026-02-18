# Logic of Scattering Matrix Calculation (Scattering.wl)

This document describes the algorithm for constructing and calculating S-parameters of superconducting circuits. Particular attention is paid to the fundamental architectural decision regarding the **separation of ground concepts** for the quantum Hamiltonian and the classical microwave S-matrix.

## 1. Fundamental Concept: "Floating Ground"

The main feature of the implementation is the refusal to use the topological grounding node (`GroundNode`) when constructing the admittance matrix.

### Problem
In the quantum description (Hamiltonian), the choice of ground (`GroundNode`) is a phase gauge ($\phi = 0$). This choice is arbitrary and necessary for the invertibility of the capacitance matrix.
However, in microwave analysis (S-Matrix), "ground" is a physical shield or cable braid. If we rigidly link the microwave ground to the quantum ground, we cannot correctly model:
* Pass-through elements (Series components) inserted into a line break.
* Floating resonators.
* Differential ports.

### Solution
The `Scattering.wl` module implements the **fully floating circuit** concept:

1.  **Ignoring GroundNode:** When building the Y-matrix, the module ignores the `GroundNode` field from the input topology.
2.  **All Nodes Active:** All circuit nodes (including the one that was "ground" in the quantum calculation) are considered equal signal nodes.
3.  **External Reference:** Potentials are assumed to be measured relative to some external "absolute" ground to which ports are connected via a load $Z_0$. Internal circuit nodes do not have a direct connection to this ground unless explicitly defined by components.

> **Result:** This allows obtaining physically correct results (e.g., $|S_{21}| = 1.0$ for a series LC circuit at resonance), since current does not "leak" into the virtual quantum ground.

---

## 2. Construction Algorithm (BuildSymbolicScattering)

The function performs the following steps to obtain a symbolic S-matrix:

### Step 1: Symbol Synchronization
`primaryParams` from `Model.wl` are supplied as input. This is critically important so that symbols ($C_1, L_{ext}$) used in the scattering matrix match the symbols used in the rest of the model.

### Step 2: Local Port Detection
Instead of using global graph properties, ports are determined locally based on the component list:
* **All** circuit nodes are analyzed (without excluding ground).
* The degree of each node is calculated (how many components are connected to it).
* **Port** — a node with degree **1** (dangling end).
* If ports are not found automatically (closed circuit), the 1st and N-th nodes of the list are used.

### Step 3: Y-Matrix Construction (Admittance Matrix)
An admittance matrix of size $N \times N$ is built, where $N$ is the total number of nodes.
* For each component (between nodes $u$ and $v$), conductance $Y_{comp}$ is added:
    $$Y_{uu} \ += Y_{comp}, \quad Y_{vv} \ += Y_{comp}$$
    $$Y_{uv} \ -= Y_{comp}, \quad Y_{vu} \ -= Y_{comp}$$
* **Important:** Since no node is excluded, the matrix describes a circuit "hanging in the air".

### Step 4: Reduction (Schur Complement)
The matrix is divided into blocks: Ports ($P$) and Internal nodes ($I$).
$$Y = \begin{pmatrix} Y_{PP} & Y_{PI} \\ Y_{IP} & Y_{II} \end{pmatrix}$$
Internal nodes are eliminated using the Schur complement method:
$$Y_{reduced} = Y_{PP} - Y_{PI} \cdot (Y_{II})^{-1} \cdot Y_{IP}$$

### Step 5: Transition to S-Matrix
The standard conversion formula for characteristic impedance $Z_0$ (usually 50 Ohm) is used:
$$S = (U - Z_0 \cdot Y_{reduced}) \cdot (U + Z_0 \cdot Y_{reduced})^{-1}$$
where $U$ is the identity matrix.

---

## 3. Role of Quantum Ground (GetEffectiveInductances)

Although `GroundNode` is ignored when building the linear S-matrix, it **returns** at the stage of numerical calculation of nonlinear inductances.

The effective inductance of a Josephson junction depends on the phase difference:
$$L_{eff} \propto \frac{1}{E_J \cos(\phi_1 - \phi_2)}$$

Here, phases $\phi_1, \phi_2$ are taken from the static calculation (`EquilibriumFluxes`), which is **rigidly linked** to the quantum ground ($\phi_{ground} = 0$).
Thus:
* **Static (DC):** Uses `GroundNode` for phase uniqueness.
* **Dynamic (RF):** Ignores `GroundNode` for correct signal propagation.

---

## 4. Visualization and Calculations

* **Linear Scale:** Plots are built in absolute magnitude $|S|$ (0...1), without logarithmic scaling. This improves performance and resonance clarity.
* **Caching:** To speed up `Manipulate` or frequency sweeps, the S-matrix is cached in a "semi-symbolic" form (numbers are substituted into everything except the frequency variable `s`).

---

**Summary:** This architecture allows modeling arbitrary topologies (shunt, series, floating) without manual "ground" configuration for each case, correctly handling the physics of microwave circuits.