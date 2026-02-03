BeginPackage["QED`Numeric`HarmonicOscillator`"];

DiagonalizeHarmonicHamiltonian::usage = 
"DiagonalizeHarmonicHamiltonian[invC, invL] performs canonical diagonalization \
of quadratic Hamiltonian H = (1/2)q^T·C^(-1)·q + (1/2)φ^T·L^(-1)·φ. \
Returns Association with normal mode transformation matrices.

Input:
  invC - Numerical inverse capacitance matrix C^(-1) [F^(-1)]
  invL - Numerical inverse inductance matrix L^(-1) [H^(-1)]

Output Association:
  \"ChargeTransform\" -> M        : q_new = M · q_old
  \"FluxTransform\" -> N          : φ_new = N · φ_old  
  \"DiagonalizedInductance\" -> j : Diagonal L^(-1) in normal modes
  \"EffectiveCapacitances\" -> Cs : Diagonal effective capacitances C_k [F]
  \"NormalModeFrequencies\" -> w  : Eigenfrequencies [rad/s]
  \"RotationMatrix\" -> d1        : Orthogonal rotation matrix

Physics: Simultaneous diagonalization preserving [q_i, φ_j] = iℏδ_ij. \
Reference: Devoret (2004), Sec. 3.3; Nigg et al., PRX 2, 041027 (2012).

Debug: Set $DebugHarmonicDiagonalization = True for detailed output.";

Begin["`Private`"];

(* ════════════════════════════════════════════════════════════════ *)
(*                       DEBUG FLAG                                 *)
(* ════════════════════════════════════════════════════════════════ *)

$DebugHarmonicDiagonalization = False;

DiagonalizeHarmonicHamiltonian[invC_?MatrixQ, invL_?MatrixQ] := 
 Module[{omega0, C0, L0, invCscaled, invLscaled, 
         M, N1, s1, s2, d1, j, Ntransform, Mtransform, signCorrection,
         jScaled, CdiagScaled, LdiagScaled, omega2Scaled, effectiveCaps},
  
  If[$DebugHarmonicDiagonalization,
    Print["=== DiagonalizeHarmonicHamiltonian ==="];
    Print["Input C^-1 dimensions: ", Dimensions[invC]];
    Print["Input L^-1 dimensions: ", Dimensions[invL]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 0: Define scales for dimensionless matrices                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  omega0 = Min[Abs[Sqrt[Eigenvalues[invC . invL]]]];  (* Use Abs for complex frequencies *)
  C0 = Max[Abs[Diagonal[Inverse[invC]]]];
  L0 = 1/(C0 * omega0^2);
  
  invCscaled = invC * C0;
  invLscaled = invL * L0;
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 0: Dimensionless scaling ---"];
    Print["omega_0 = ", ScientificForm[omega0, 4], " rad/s (minimum |omega|)"];
    Print["C_0 = ", ScientificForm[C0, 4], " F"];
    Print["L_0 = ", ScientificForm[L0, 4], " H"];
    Print["Scaled C^-1 and L^-1 are now dimensionless [1]"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 1: Diagonalize dimensionless C^(-1)                         *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  {s1, s2} = JordanDecomposition[invCscaled];
  M = s1 . Inverse[Chop[Sqrt[s2]]];
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 1: Diagonalize scaled C^-1 ---"];
    Print["M = s1 * (sqrt(s2))^-1 (dimensionless)"];
    Print["Check: M^T * C^-1 * M diagonal?"];
    Print[MatrixForm[Transpose[M] . invCscaled . M]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 2: Canonical conjugate transformation                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  N1 = s1 . Chop[Sqrt[s2]];
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 2: Conjugate transformation ---"];
    Print["N1 = s1 * sqrt(s2) (dimensionless)"];
    Print["Check symplectic M^T * N1 = I:"];
    Module[{check, err},
      check = Transpose[M] . N1;
      err = Max[Abs[check - IdentityMatrix[Length[M]]]];
      Print[MatrixForm[check]];
      Print["Error from identity: ", ScientificForm[err, 3]];
    ];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 3: Diagonalize transformed dimensionless L^(-1)             *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  {d1, j} = JordanDecomposition[Transpose[N1] . invLscaled . N1];
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 3: Diagonalize N1^T * L^-1 * N1 ---"];
    Print["j (dimensionless diagonal): ", MatrixForm[j]];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 4: Apply rotation and sign correction                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  signCorrection = DiagonalMatrix[Sign[Diagonal[Inverse[N1 . d1]]]];
  Ntransform = N1 . d1 . signCorrection;
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 5: Derive M from commutation relation                       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  Mtransform = Inverse[Transpose[Ntransform]];
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 4-5: Rotation and symplectic derivation ---"];
    Print["N = N1 * d1 * sign (dimensionless)"];
    Print["M = (N^T)^-1 (dimensionless)"];
    Print["Sign correction: ", Diagonal[signCorrection]];
    Print["[M] = [N] = [1] => q' and phi' keep physical dimensions!"];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Step 6: Restore physical dimensions for matrices and frequencies *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  jScaled = j / L0;
  
  CdiagScaled = Transpose[Mtransform] . invC . Mtransform;
  LdiagScaled = Transpose[Ntransform] . invL . Ntransform;
  omega2Scaled = Diagonal[CdiagScaled] * Diagonal[LdiagScaled];
  
  (* Calculate effective capacitances: C_k = 1 / (M^T C^-1 M)_kk *)
  effectiveCaps = 1.0 / Diagonal[CdiagScaled];
  
  If[$DebugHarmonicDiagonalization,
    Print["\n--- Step 6: Restore physical dimensions ---"];
    Print["j_physical = j / L_0 (dimension [H^-1])"];
    Print["omega^2 computed from physical C^-1 and L^-1"];
    Print["Effective Caps: ", effectiveCaps];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* FINAL VALIDATION                                                 *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  If[$DebugHarmonicDiagonalization,
    Print["\n=== FINAL VALIDATION ==="];
    
    (* 1. Diagonal L^-1 *)
    Module[{Ldiag, err},
      Ldiag = LdiagScaled;
      err = Max[Abs[Ldiag - DiagonalMatrix[Diagonal[Ldiag]]]] / Max[Abs[Diagonal[Ldiag]]];
      Print["1. N^T * L^-1 * N diagonal? Error: ", ScientificForm[err, 3]];
      Print["   Diagonal (H^-1): ", ScientificForm[#, 4]& /@ Diagonal[Ldiag]];
    ];
    
    (* 2. Diagonal C^-1 *)
    Module[{Cdiag, err},
      Cdiag = CdiagScaled;
      err = Max[Abs[Cdiag - DiagonalMatrix[Diagonal[Cdiag]]]] / Max[Abs[Diagonal[Cdiag]]];
      Print["2. M^T * C^-1 * M diagonal? Error: ", ScientificForm[err, 3]];
      Print["   Diagonal (F^-1): ", ScientificForm[#, 4]& /@ Diagonal[Cdiag]];
    ];
    
    (* 3. Frequencies *)
    Print["3. Normal mode frequencies:"];
    Print["   omega (rad/s): ", ScientificForm[#, 4]& /@ Sqrt[omega2Scaled]];
    Print["   f (GHz): ", ScientificForm[#, 4]& /@ (Sqrt[omega2Scaled] / (2*Pi*10^9))];
    
    (* 4. Commutation relation *)
    Print["\n4. Commutation relation {q', phi'} = hbar*delta:"];
    Module[{commutator, expected, absError},
      commutator = Mtransform . Transpose[Ntransform];
      expected = IdentityMatrix[Length[Mtransform]];
      absError = Max[Abs[commutator - expected]];
      
      Print["   M * N^T (dimensionless, should be I):"];
      Print["   ", MatrixForm[commutator]];
      Print["   Absolute error: ", ScientificForm[absError, 3]];
      Print["   Pass (< 10^-10)? ", absError < 10^-10];
      Print["\n   Physical dimensions preserved:"];
      Print["   [M] = [N] = [1] (dimensionless)"];
      Print["   [q'] = [M]*[q] = [C] (Coulombs)"];
      Print["   [phi'] = [N]*[phi] = [Wb] (Webers)"];
      Print["   [q', phi'] = i*hbar*delta where hbar = ", 
            ScientificForm[QED`$hbarValue, 4], " J*s"];
    ];
    
    (* 5. Frequency consistency check *)
    Print["\n=== FREQUENCY CONSISTENCY CHECK ==="];
    Module[{omegaFromEigenvalues, omegaFromDiag, relError},
      omegaFromEigenvalues = Sort[Sqrt[Eigenvalues[invC . invL]], Abs[#1] < Abs[#2] &];
      omegaFromDiag = Sort[Sqrt[omega2Scaled], Abs[#1] < Abs[#2] &];
      relError = Max[Abs[omegaFromEigenvalues - omegaFromDiag] / Abs[omegaFromEigenvalues]];
      
      Print["From eigenvalues(C^-1 * L^-1): ", ScientificForm[#, 4]& /@ omegaFromEigenvalues, " rad/s"];
      Print["From diagonalization (M, N):   ", ScientificForm[#, 4]& /@ omegaFromDiag, " rad/s"];
      Print["Max relative error: ", ScientificForm[relError, 3]];
      Print["Pass (< 10^-10)? ", relError < 10^-10];
    ];
  ];
  
  (* ════════════════════════════════════════════════════════════════ *)
  (* Return dimensionless transformations and physical matrices       *)
  (* ════════════════════════════════════════════════════════════════ *)
  
  <|
    "ChargeTransform" -> Mtransform,
    "FluxTransform" -> Ntransform,
    "DiagonalizedInductance" -> jScaled,
    "EffectiveCapacitances" -> effectiveCaps,
    "NormalModeFrequencies" -> Sqrt[omega2Scaled],
    "RotationMatrix" -> d1,
    "Scales" -> <|
      "Frequency" -> omega0,
      "Capacitance" -> C0,
      "Inductance" -> L0
    |>
  |>
];



End[];
EndPackage[];