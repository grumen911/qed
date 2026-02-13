# Symbolic Anharmonicity Calculation and Perturbation Theory

This document describes the methodology and software implementation for calculating energy level corrections in superconducting circuits caused by the nonlinearity of Josephson junctions.

## 1. Theoretical Background

The Hamiltonian of a superconducting circuit can be represented as the sum of a harmonic part and a perturbation:
$$\hat{H} = \hat{H}_{harm} + \hat{V}_{anh}$$

Where $\hat{H}_{harm}$ is the quadratic Hamiltonian, diagonalized by transitioning to normal modes, and $\hat{V}_{anh}$ contains higher-order Taylor expansion terms of the potential (cubic, quartic, etc.).

### Potential Expansion
The potential energy of a Josephson junction $U(\phi) = -E_J \cos(\phi/\phi_0)$ is expanded in a Taylor series around the equilibrium point $\phi_{\min}$:

$$U(\phi) \approx \frac{1}{2}U''(\delta\phi)^2 + \underbrace{\frac{1}{6}U'''(\delta\phi)^3 + \frac{1}{24}U''''(\delta\phi)^4 + \dots}_{\hat{V}_{anh}}$$

* **$\phi^3$ terms**: These appear if $\phi_{\min} \neq 0$ (e.g., in the presence of external magnetic flux). In first-order perturbation theory for diagonal elements, they contribute zero (as they contain an odd number of creation/annihilation operators).
* **$\phi^4$ terms**: The primary source of anharmonicity (Self-Kerr and Cross-Kerr).

### Quantization and Operators
The transition from fluxes to creation ($\hat{a}^\dagger$) and annihilation ($\hat{a}$) operators is performed in two stages:
1.  **Linear Transformation**: $\vec{\phi} = \vec{\phi}_{\min} + T \cdot \vec{q}$, where $T$ is the normal mode matrix.
2.  **Second Quantization**: $q_k = \phi_{ZPF, k} (\hat{a}_k + \hat{a}_k^\dagger)$.

### Energy Correction (1st Order)
The energy shift of level $|n\rangle$ is calculated as the expectation value of the perturbation operator:
$$\Delta E_n = \langle n | \hat{V}_{anh} | n \rangle$$

We **do not** perform preliminary symbolic Normal Ordering, as it is computationally expensive for multi-mode circuits. Instead, we apply the unordered operator (e.g., $(\hat{a}+\hat{a}^\dagger)^4$) directly to the state vector.

#### Physical Interpretation of Contributions
After expansion, a term like $(\hat{a}+\hat{a}^\dagger)^4$ contains summands that preserve the particle number (equal count of $a^\dagger$ and $a$):

1.  **$\hat{a}^\dagger \hat{a}^\dagger \hat{a} \hat{a} = \hat{n}(\hat{n}-1)$**:
    * This is the **true anharmonicity** (Self-Kerr).
    * For states $|0\rangle$ and $|1\rangle$, it equals **0**.
    * It becomes non-zero only starting from $|2\rangle$.
2.  **$\hat{a}^\dagger \hat{a} = \hat{n}$**:
    * Arises from commutation relations $[\hat{a}, \hat{a}^\dagger]=1$.
    * This is **frequency renormalization**. It shifts the $|1\rangle$ level but depends linearly on $n$.
3.  **Constant**:
    * Vacuum energy shift.

**Note:** For single-photon states (e.g., $|001\rangle$), the anharmonic correction $\Delta E$ is purely frequency renormalization. The qubit anharmonicity parameter $\alpha$ is defined as the difference between transition frequencies:
$$\alpha = (E_2 - E_1) - (E_1 - E_0)$$

---

## 2. Code Implementation (Analytic.wl)

The `QED`Analytic` module provides three key functions for this calculation.

### `BuildAnharmonicPart`
```wolfram
BuildAnharmonicPart[hamiltonian, topology, order]
```

* **Purpose:** Symbolically extracts the non-quadratic part of the potential.
* **Logic:** Uses a variable scaling method ($t \cdot \delta\phi$) to correctly truncate the Taylor series by the total degree of variables (Total Degree Truncation).
* **Feature:** Returns a polynomial of symbolic fluxes $\phi_i$ and their equilibrium values $\phi_{\min, i}$. Numerical values of $\phi_{\min}$ must be substituted before further use.

### `QuantizeToLadderOperators`

```wolfram
QuantizeToLadderOperators[expr, topology, transformMatrix, freqs, caps]
```

* **Purpose:** Converts a flux expression into a polynomial of non-commutative `Create` and `Annihilate` operators.
* **Logic:**
1. Replaces $\phi \to T \cdot q$.
2. Replaces $q \to x_{ZPF} (a + a^\dagger)$.
3. **Crucial:** Converts `Power[x, n]` into non-commutative sequences `x ** x ** ...` to preserve operator ordering.



### `CalculatePerturbationCorrection`

```wolfram
CalculatePerturbationCorrection[operator, state]
```

* **Purpose:** Computes the diagonal matrix element $\langle n | \hat{V} | n \rangle$.
* **Logic:** Iteratively applies operators to the occupation number vector from right to left, tracking the normalization factors $\sqrt{n}$ and $\sqrt{n+1}$.

