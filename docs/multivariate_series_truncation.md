# Multivariate Series truncation (WL): per-variable vs total-degree

This note documents a subtle but important difference between:
- Per-variable Series in Wolfram Language (successive expansion in each variable).
- Total-degree Taylor truncation around a point (truncation by total degree in the increments).

The mismatch matters because it can change the apparent local geometry (minimum vs saddle)
and can shift the minimizer of the truncated polynomial.

## Key idea

`Series[f, {x, x0, nx}, {y, y0, ny}]` performs series expansions successively with respect
to `x`, then `y`, etc.
This corresponds to a “rectangular” truncation: the degree in each variable is bounded separately.

A true multivariate Taylor polynomial of total degree ≤ N is different: it keeps only monomials
whose total degree in the increments is ≤ N.

See discussion and examples:
https://stackoverflow.com/questions/7747596/multivariate-taylor-series-expansion-in-mathematica

## Demo script

The script `series_truncation_demo.wl` prints three related polynomials.

### 1) Per-variable Series (rectangular truncation)

`serPerVarExpr` is produced directly by:

~~~wl
serPerVarExpr =
  Normal@Series[U[x, y], {x, p1m, nPerVar}, {y, p2m, nPerVar}] /. {x -> p1, y -> p2} // Expand;
~~~
This polynomial may include terms such as `p1^2p2`, `p1p2^2`, `p1^2*p2^2`, etc., depending on `nPerVar`.

2) “Shadows”: keep only monomials of degree ≤ 2 in `(p1, p2)`
Sometimes it is useful to explicitly drop all monomials with total degree > 2 in the raw variables
`(p1, p2)` while keeping the lower-degree terms that remain after expansion (informally: “leave only shadows”).

This is not the same as truncation in increments around `(p1m, p2m)`; it is just a filter in `(p1, p2)`.

~~~wl
serPerVarExprShadow2 =
  FromCoefficientRules[
    Select[CoefficientRules[serPerVarExpr, {p1, p2}], Total[First[#]] <= 2 &],
    {p1, p2}
  ] // Expand;
~~~
3) Total-degree Taylor via t-scaling
`serTotalDegExpr` is the Taylor polynomial around `(p1m, p2m)` obtained by scaling the increment with a single parameter t:

~~~wl
serTotalDegExpr =
 Module[{d1 = p1 - p1m, d2 = p2 - p2m},
  Normal@Series[U[p1m + t d1, p2m + t d2], {t, 0, nTotalDeg}] /. t -> 1 // Expand
 ];
~~~
This keeps terms of total degree ≤ `nTotalDeg` in the increment `(d1, d2)`.

Comparing “shadows” to total-degree
We compare the “shadows” polynomial (degree ≤ 2 in raw variables) with the true total-degree quadratic Taylor polynomial:

~~~wl
diff = Chop[serPerVarExprShadow2 - serTotalDegExpr];
~~~
If `diff != 0`, it does not mean the expansion point is wrong.
It means the two objects are different by construction:

`serPerVarExprShadow2` filters monomials by degree in `(p1, p2)`.

`serTotalDegExpr` truncates by degree in the increment `(p - pm)`.

These operations do not commute with shifting the origin, so they generally produce different quadratics.