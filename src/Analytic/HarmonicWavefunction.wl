BeginPackage["QED`Analytic`HarmonicWavefunction`"];

BuildHarmonicWavefunction::usage = 
"BuildHarmonicWavefunction[fluxVars, minFluxVars, frequencies, transformationMatrix, quantumNumbers] \
constructs the analytical harmonic oscillator wavefunction in the original flux coordinates.

Arguments:
  fluxVars: List of symbolic flux variables {φ₁, φ₂, ...} (physical units)
  minFluxVars: List of equilibrium flux symbols {φ₁,min, φ₂,min, ...}
  frequencies: List of normal mode frequencies {ω₁, ω₂, ...}
  transformationMatrix: Matrix T such that δφ = T · q_normal
  quantumNumbers: List of integers {n₁, n₂, ...} specifying the state

Returns:
  Symbolic expression Ψ(φ₁, φ₂, ...). Includes Jacobian normalization factor.";

Begin["`Private`"];

(* 
   Implementation of 1D Harmonic Oscillator Wavefunction 
   ψ_n(x) = N_n * H_n(α*x) * exp(-α^2 * x^2 / 2)
   
   Here x is the normal coordinate q_k.
   Scaling factor α_k = Sqrt[C_k * ω_k / ℏ]
   But since we work in a diagonalized basis where H = sum ℏω(a†a+1/2),
   the effective mass/capacitance is implicitly handled by the transformation.
   
   However, we need to be careful with units.
   Let's look at the dimensionless coordinates used in Quantum Mechanics.
   ξ = x * sqrt(mω/ℏ).
   
   From our Diagonalization procedure (HarmonicOscillator.wl):
   We obtained T such that Hamiltonian is diagonal.
   Let's rely on the standard result:
   The ground state is a Gaussian.
   Ψ_0(q) ~ Exp[- (1/2) q^T · (something) · q]
   
   Let's use the explicit formula for a mode k with frequency ω_k.
   The coordinate q_k has dimensions of Flux [Φ].
   The effective capacitance of this mode is C_k.
   
   Wait, in our Normal Mode analysis (HarmonicOscillator.wl), 
   we produce a transformation such that the Hamiltonian becomes:
   H = (1/2) ∑ (C_k^{-1} q_charge_k^2 + L_k^{-1} q_flux_k^2)
     = (1/2) ∑ ( (1/C_k) p_k^2 + (1/L_k) x_k^2 )
   
   This is a harmonic oscillator with mass m = C_k and spring constant k = 1/L_k.
   Frequency ω_k = 1/sqrt(L_k C_k).
   
   The dimensionless coordinate is ξ_k = x_k * sqrt(m ω_k / ℏ) = x_k * sqrt(C_k ω_k / ℏ).
   
   Where do we get C_k? 
   In HarmonicOscillator.wl, we calculate:
   CdiagScaled = M^T . invC . M
   This is the inverse capacitance matrix in the normal basis. It is diagonal.
   So (1/C_k) = (CdiagScaled)_kk.
   Therefore C_k = 1 / (CdiagScaled)_kk.
   
   But BuildHarmonicWavefunction receives `transformationMatrix` (T) and `frequencies`.
   It does NOT receive C_k directly.
   
   Alternative approach:
   We know that for the ground state of a multi-dimensional oscillator:
   Ψ_0(δφ) = (det(Re[Ω]))^(1/4) / (π^(N/4)) * Exp[ -1/2 * δφ^T · Ω · δφ ]
   where Ω is the covariance matrix related to the vacuum fluctuations.
   
   Let's stick to the separation of variables in normal coordinates, which is robust.
   Ψ(q) = Π ψ_nk(qk)
   
   We need the "width" of the oscillator function.
   Let's assume the provided `transformationMatrix` T normalizes the kinetic energy term properly?
   
   Let's check `HarmonicOscillator.wl` again.
   It ensures [q', phi'] = i*hbar.
   And it diagonalizes H.
   
   If we use the creation/annihilation operators defined by the diagonalization:
   a_k = ...
   
   Let's look at the standard QED convention (Clerk et al, RMP):
   φ_k = φ_zpf,k * (a_k + a_k^†)
   
   In the normal basis q_k:
   q_k = φ_zpf,k * (a_k + a_k^†)
   
   The ground state wavefunction for coordinate q_k has width σ_k = φ_zpf,k.
   ψ_0(q_k) = (2π σ_k^2)^(-1/4) * Exp[ - q_k^2 / (4 σ_k^2) ]
   
   We need φ_zpf,k.
   For a mode with impedance Z_k = 1/(ω_k C_k),
   φ_zpf,k = Sqrt[ℏ Z_k / 2] = Sqrt[ℏ / (2 ω_k C_k)].
   
   We need C_k. 
   We can re-calculate it from T and the original C matrix?
   Or pass it as an argument?
   
   Let's simplify.
   We assume the input `transformationMatrix` T converts δφ to `Normal Coordinates` q_k.
   δφ = T . q
   
   If HarmonicOscillator.wl does its job, the Hamiltonian in q_k is:
   H = ∑ ( p_k^2 / (2 C_k) + C_k ω_k^2 q_k^2 / 2 )  <-- verifying mass term
   
   Actually, HarmonicOscillator.wl returns T (FluxTransform) and M (ChargeTransform).
   And checks M . T^T = I.
   
   It calculates:
   Cdiag = M^T . invC . M  (Inverse Capacitance, diagonal)
   Ldiag = N^T . invL . N  (Inverse Inductance, diagonal)
   
   So Hamiltonian is:
   H = 1/2 q_charge^T . Cdiag . q_charge + 1/2 q_flux^T . Ldiag . q_flux
     = ∑ 1/2 (Cdiag_kk * p_k^2 + Ldiag_kk * x_k^2)
   
   Identifying with H = p^2/(2m) + 1/2 m ω^2 x^2:
   1/(2m) = 1/2 * Cdiag_kk  =>  m_k = 1/Cdiag_kk = C_k (Effective Capacitance)
   m ω^2 = Ldiag_kk        =>  C_k ω_k^2 = Ldiag_kk
   
   Check consistency: ω_k^2 = Ldiag_kk / Cdiag_kk. This matches ω^2 = eig(C^-1 L^-1). Correct.
   
   So, we NEED `Cdiag` (or effective capacitances) to build the wavefunction correctly.
   Just `T` and `ω` is not enough, because `T` (N) determines Ldiag, but M determines Cdiag.
   Although they are linked by symplectic property, we shouldn't re-derive M here.
   
   PROPOSAL:
   Add `effectiveCapacitances` as an argument to this function.
   
   Width parameter α_k (inverse length):
   Argument of HermiteH is (x / x_zpf).
   x_zpf = Sqrt[ℏ / (2 m ω)] = Sqrt[ℏ / (2 C_k ω_k)].
   
   Dimensionless coordinate:
   ξ_k = q_k / (Sqrt[2] * x_zpf) ? No.
   
   Standard physics definition:
   ψ_n(x) = (1/sqrt(2^n n!)) * (mω/πℏ)^(1/4) * Exp[-mω x^2 / (2ℏ)] * H_n( sqrt(mω/ℏ) x )
   
   Let's denote invLength_k = sqrt(C_k ω_k / ℏ).
   Then argument is (invLength_k * q_k).
   Gaussian is Exp[ - (invLength_k * q_k)^2 / 2 ].
   Prefactor is (invLength_k^2 / π)^(1/4).
   
   So we need:
   - effectiveCapacitances {C₁, C₂, ...}
   - frequencies {ω₁, ω₂, ...}
   - transformation T (to get q_k from δφ)
   
   Note: φ_min is just a shift.
*)

(* Constants *)
hbar = QED`$hbar; (* Symbol, assumed defined in QED package *)


BuildHarmonicWavefunction[
    fluxVars_List, 
    minFluxVars_List, 
    frequencies_List, 
    effectiveCapacitances_List, 
    transformationMatrix_?MatrixQ, 
    quantumNumbers_List
] := Module[{
    deltaPhi,     (* Vector of flux deviations *)
    normalCoords, (* Vector of normal coordinates q *)
    invT,         (* Inverse transformation matrix *)
    detT,         (* Jacobian determinant *)
    wavefunctions1D,
    psiTotal,
    nDOF
},
    nDOF = Length[fluxVars];
    
    (* 1. Coordinate Transformation *)
    (* δφ = T . q  =>  q = T^-1 . δφ *)
    
    deltaPhi = fluxVars - minFluxVars;
    
    (* Inverse of T to express q in terms of φ *)
    invT = Inverse[transformationMatrix];
    normalCoords = invT . deltaPhi;
    
    (* Jacobian of transformation φ -> q *)
    (* dφ = |det T| dq *)
    (* We need normalization ∫|ψ|² dφ = 1 *)
    (* ∫|ψ(q)|² |det T| dq = 1 *)
    (* If ψ(q) is normalized to ∫|ψ(q)|² dq = 1, then we need factor 1/sqrt(|det T|) *)
    
    detT = Abs[Det[transformationMatrix]];
    
    (* 2. Build 1D wavefunctions for each mode *)
    wavefunctions1D = MapThread[
        Function[{q, n, omega, cap},
            Module[{invLength2, normFactor, gaussian, poly, xi},
                (* m = cap, freq = omega *)
                (* α² = mω/ℏ *)
                
                invLength2 = (cap * omega) / hbar;
                xi = Sqrt[invLength2] * q;
                
                (* Normalization factor for 1D oscillator *)
                (* (α²/π)^(1/4) / sqrt(2^n n!) *)
                normFactor = (invLength2 / Pi)^(1/4) / Sqrt[2^n * Factorial[n]];
                
                gaussian = Exp[-invLength2 * q^2 / 2];
                poly = HermiteH[n, xi];
                
                normFactor * gaussian * poly
            ]
        ],
        {normalCoords, quantumNumbers, frequencies, effectiveCapacitances}
    ];
    
    (* 3. Combine *)
    psiTotal = (1 / Sqrt[detT]) * Times @@ wavefunctions1D;
    
    psiTotal
];

End[];
EndPackage[];