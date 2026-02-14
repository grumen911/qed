# PHYSICS.md: Derivation of Decoherence Rates

This document outlines the rigorous physical derivation of the relaxation ($T_1$) and dephasing ($T_\varphi$) rates for the superconducting bridge circuit. These derivations serve as the specification for the numerical implementation in `Numeric.wl`.

## 1. Interaction Hamiltonian and Noise

The superconducting circuit interacts with the electromagnetic environment (control lines, readout resonators) via two primary channels:

1. **Inductive Coupling:** Interaction of the circuit's loop current with magnetic flux fluctuations.
2. **Capacitive Coupling:** Interaction of the node charge with voltage fluctuations.

### 1.1. The Current Operator Definition

In a multi-node circuit where the choice of ground is arbitrary, the "current" flowing through a loop cannot be simply defined by a single node flux. It must be defined thermodynamically.

Let the system Hamiltonian be $\hat{H}(\Phi_{\text{ext}})$, where $\Phi_{\text{ext}}$ is the external magnetic flux threading the circuit loop. The operator for the circulating current $\hat{I}_{\text{circ}}$ is defined as:

$$\hat{I}_{\text{circ}} = - \frac{\partial \hat{H}}{\partial \Phi_{\text{ext}}}$$

**Why this is necessary:**
In the bridge circuit Hamiltonian, the external flux $\Phi_{\text{ext}}$ typically enters as a phase shift in the cosine potential of specific junctions. For example, if the flux threads a loop involving junctions between nodes $i$ and $j$:

$$\hat{H} = \sum \frac{\hat{q}_k^2}{2C} - \sum E_{J,k} \cos(\hat{\varphi}_k)$$

If $\Phi_{\text{ext}}$ is assigned to the branch $1 \to 2$, the potential term is $-E_{J1} \cos(\frac{\hat{\phi}_1 - \hat{\phi}_2 + \Phi_{\text{ext}}}{\phi_0})$. The derivative yields:

$$\hat{I}_{\text{circ}} = -\frac{\partial \hat{H}}{\partial \Phi_{\text{ext}}} = \frac{E_{J1}}{\phi_0} \sin\left(\frac{\hat{\phi}_1 - \hat{\phi}_2 + \Phi_{\text{ext}}}{\phi_0}\right)$$

In the harmonic approximation (linearized regime), this simplifies to the current flowing through the effective inductance of that branch:

$$\hat{I}_{\text{circ}}^{\text{lin}} \approx \frac{\hat{\phi}_1 - \hat{\phi}_2}{L_{J1}(\Phi_{\text{ext}})}$$

> **Implementation Note:** In the code, we must calculate the matrix element of this specific operator difference (or the numerical derivative of the Hamiltonian), rather than using the flux of a single node $\hat{\phi}_1$.

### 1.2. The Role of Equilibrium Fluxes ($\bm{\phi}_{\text{eq}}$)

The equilibrium flux configuration $\bm{\phi}_{\text{eq}}$ (where $\nabla U(\bm{\phi}) = 0$) plays a dual role: it defines the operating point of the qubit and determines the **effective coupling strength** to the noise source.

The full non-linear current operator through a Josephson junction with critical current $I_c$ is:

$$\hat{I}_{\text{JJ}} = I_c \sin\left( \frac{\hat{\phi}_i - \hat{\phi}_j + \Phi_{\text{ext}}}{\phi_0} \right)$$

In the harmonic approximation used by the code, we decompose the node fluxes into a classical equilibrium part and a quantum fluctuation part: $\hat{\phi} = \phi_{\text{eq}} + \delta\hat{\phi}$. Expanding the current operator to the first order in $\delta\hat{\phi}$:

$$\hat{I}_{\text{JJ}} \approx I_c \sin(\varphi_{\text{dc}}) + \underbrace{\left[ \frac{I_c}{\phi_0} \cos(\varphi_{\text{dc}}) \right] (\delta\hat{\phi}_i - \delta\hat{\phi}_j)}_{\text{Noise Coupling Operator}}$$

Here, $\varphi_{\text{dc}} = (\phi_{i,\text{eq}} - \phi_{j,\text{eq}} + \Phi_{\text{ext}})/\phi_0$ is the total phase bias across the junction.

**Crucial Implications for Protection:**

1. **DC Term:** The first term is a static current expectation value. It shifts the energy but does not induce transitions ($T_1$ processes).
2. **Coupling Coefficient:** The prefactor $\alpha = \frac{I_c}{\phi_0} \cos(\varphi_{\text{dc}})$ acts as a tunable coupling constant.
* This coefficient is effectively the inverse kinetic inductance: $\alpha = 1/L_J(\bm{\phi}_{\text{eq}})$.
* **Protection Mechanism:** If the circuit parameters and flux bias are tuned such that $\cos(\varphi_{\text{dc}}) \to 0$ (or if interference cancels this term globally across multiple junctions), the coupling to the external noise vanishes.



**Conclusion:** The equilibrium search (`FindPotentialMinimum`) is not just for finding eigenfrequencies; it is strictly necessary to calculate the correct prefactors for the noise operators. Calculating matrix elements of $\delta\hat{\phi}$ without this $\cos(\varphi_{\text{dc}})$ weight would yield incorrect relaxation rates.

---

## 2. Depolarization Rates ($T_1$)

Relaxation occurs when the qubit transitions from $|1\rangle \to |0\rangle$ by emitting energy into the noise source. We use Fermi's Golden Rule:

$$\Gamma_{1} = \frac{1}{\hbar^2} |\langle 0 | \hat{H}_{\text{int}} | 1 \rangle|^2 S_{\text{noise}}(\omega_{01})$$

### 2.1. Inductive (Flux) Channel

* **Coupling:** Via mutual inductance $M$ to a bias line with current $I_{\text{bias}}$.
* **Noise Source:** Current fluctuations in the line $\delta I_{\text{bias}}$.
* **Interaction:** $\hat{H}_{\text{int}} = - \hat{I}_{\text{circ}} \delta \Phi_{\text{noise}} = - \hat{I}_{\text{circ}} (M \delta I_{\text{bias}})$.

The spectral density of current noise in a resistor $R$ (at low $T$) is $S_{I}(\omega) = \frac{2\hbar\omega}{R}$.

$$\Gamma_{\text{ind}} = \frac{M^2}{\hbar^2} |\langle 0 | \hat{I}_{\text{circ}} | 1 \rangle|^2 S_{I}(\omega_{01})$$

Substituting $S_I$:

$$\Gamma_{\text{ind}} = \frac{2 \omega_{01} M^2}{\hbar R} \left| \langle 0 | \hat{I}_{\text{circ}} | 1 \rangle \right|^2$$

If we approximate $\hat{I}_{\text{circ}} \approx \hat{\phi}_{\text{eff}} / L_{\text{loop}}$, this becomes:

$$\boxed{ \Gamma_{\text{ind}} = \frac{2 \omega_{01} M^2}{\hbar R L_{\text{loop}}^2} \left| \langle 0 | \hat{\phi}_{\text{eff}} | 1 \rangle \right|^2 }$$

* **Dependence:** $\Gamma \propto \omega \cdot |\phi|^2 \propto \omega \cdot \frac{1}{\omega} \to \text{Const}$ (roughly constant over frequency, or linear if considering $M(\omega)$ effects).

### 2.2. Capacitive (Charge) Channel

* **Coupling:** Via capacitor $C_c$ to a voltage line.
* **Noise Source:** Voltage fluctuations $\delta V$.
* **Interaction:** $\hat{H}_{\text{int}} = \hat{q}_{\text{node}} \delta V_{\text{eff}}$. Assuming $C_c \ll C_{\text{node}}$, $\delta V_{\text{eff}} \approx \frac{C_c}{C_{\text{node}}} \delta V$.

The spectral density of voltage noise is $S_V(\omega) = 2\hbar\omega R$.

$$\Gamma_{\text{cap}} = \frac{1}{\hbar^2} \left( \frac{C_c}{C_{\text{node}}} \right)^2 |\langle 0 | \hat{q}_{\text{node}} | 1 \rangle|^2 S_V(\omega_{01})$$

Substituting $S_V$:

$$\boxed{ \Gamma_{\text{cap}} = \frac{2 R C_c^2 \omega_{01}}{\hbar C_{\text{node}}^2} \left| \langle 0 | \hat{q}_{\text{node}} | 1 \rangle \right|^2 }$$

* **Dependence:** $\Gamma \propto \omega \cdot |q|^2 \propto \omega \cdot \omega \to \omega^2$ (or $\omega^3$ depending on specific admittance).

---

## 3. Pure Dephasing ($T_\varphi$)

Pure dephasing arises from the fluctuation of the transition frequency $\omega_{01}$ due to low-frequency noise (typically $1/f$) in the external flux $\Phi_{\text{ext}}$.

$$\delta \omega_{01}(t) = \frac{\partial \omega_{01}}{\partial \Phi} \delta \Phi(t) + \frac{1}{2} \frac{\partial^2 \omega_{01}}{\partial \Phi^2} (\delta \Phi(t))^2 + \dots$$

The dephasing rate is defined by the decay of the off-diagonal density matrix element $\rho_{01}(t) \propto e^{-\Gamma_\varphi t}$ (or $e^{-\chi(t)}$ for non-exponential decay).

### 3.1. First and Second Order Contributions

For $1/f$ noise with amplitude $A_\Phi$ (typically $\sim 10^{-6} \Phi_0$):

1. **First Order (Linear):** Dominates away from "sweet spots".

$$\Gamma_{\varphi, 1} \approx A_\Phi \left| \frac{\partial \omega_{01}}{\partial \Phi_{\text{ext}}} \right| \sqrt{2 \ln(\dots)}$$

2. **Second Order (Quadratic):** Dominates at BIC/Sweet spots where first derivative vanishes.

$$\Gamma_{\varphi, 2} \approx A_\Phi^2 \left| [cite_start]\frac{\partial^2 \omega_{01}}{\partial \Phi_{\text{ext}}^2 \text{[cite: 1, 98]}} \right| \cdot (\text{const})$$

### 3.2. Combined Numerical Formula

For the code implementation, we treat these as independent rates added in quadrature (approximation for Gaussian noise):

$$\boxed{ T_\varphi^{-1} \approx \sqrt{ \left( C_1 A_\Phi \frac{\partial \omega_{01}}{\partial \Phi} \right)^2 + \left( C_2 A_\Phi^2 \frac{\partial^2 \omega_{01}}{\partial \Phi^2} \right)^2 } }$$

Where $C_1, C_2$ are factors depending on the specific noise integration bandwidth (typically $\sim 1$ to $3$).

---

## 4. Summary for Code Implementation

1. **Current Operator:** Must be calculated as $\partial H / \partial \Phi$. In the linear regime, this corresponds to the flux difference across the junction with the $\Phi_{\text{ext}}$ term.
2. **Inductive Rate:** MUST use the Flux matrix element (specifically, the difference $\phi_i - \phi_j$) and loop inductance.
3. **Capacitive Rate:** MUST use the **Charge** matrix element.
4. **Dephasing:** Must include the **Second Derivative** term to correctly capture the BIC plateau.