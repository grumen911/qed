# qed
For calculation of supercondacting quantum circuits

## Equilibrium Flux Computation

Two methods available for finding equilibrium flux configurations:

### FindPotentialMinimum (default)
- Global optimization using NMinimize
- Fast for simple systems (~0.6 ms)
- May jump between solution branches

### FindPotentialMinimumContinuation (experimental)
- Homotopy continuation with Newton-Raphson
- Tracks specific solution branch
- Execution time: ~4 ms (dominated by symbolic differentiation)
- Access via: `GetNumericalQuantity[model, "EquilibriumFluxesContinuation"]`

Future optimization: Cache symbolic derivatives (expected 8× speedup)
