(* ::Package:: *)

BeginPackage["MainDefinitions`"]

f::usage= "f[r_, t_, n_] is power function defined for f=n^r, defined for n >= t, otherwise is 0. "

DiscreteConvf::usage= "Gives discrete convolution of funtion f[r, t, n]. "

ContinuousConvf::usage= "Gives cont. convolution of funtion f[r, t, n]. "

CoeffA::usage= "CoeffA[n,k] produces the k-th coefficient A in n-th row."

Q::usage= "Q gives convolution-like sum of power."

L::usage= "L[m, n, k] gives polynomial L."

X::usage= "X[m_, t_, a_, b_] gives coefficient X."

H::usage= "H[m_, t_, k_] gives coefficient H."

P::usage= "P[m_, n_, a_, b_] gives the polynomial P."

s::usage= "s[n_, k_] gives the coefficient s."

S::usage= "S gives an ordinary power sum."

Begin["`Private`"]

Unprotect[Power];

Power[0|0., 0|0.] = 1;

Protect[Power];

f[r_, t_, n_] := n^r Boole[n >= t];

S[p_, n_]:= Sum[k^p, {k, 0, n-1}];

DiscreteConvf[r_, t_, n_]:= Sum[f[r, t, n-k] * f[r, t, k],{k, -Infinity, +Infinity}];

ContinuousConvf[r_, t_, n_]:= Integrate[f[r, t, n-k] * f[r, t, k], {k, -Infinity, +Infinity}];

CoeffA[n_, k_] := 0;
CoeffA[n_, k_] := (2 k + 1) * Binomial[2 k, k] * Sum[CoeffA[n, j] * Binomial[j, 2 k + 1]*(-1)^(j - 1)/(j - k) * BernoulliB[2 j - 2 k], {j, 2 k + 1, n}] /; 0 <= k < n;
CoeffA[n_, k_] := (2 n + 1) * Binomial[2 n, n] /; k == n;

s[n_, k_] := k(n-k);

Q[r_, n_, a_, b_] := Sum[s[n,k]^r, {k, a, b-1}];

L[m_, n_, k_] := Sum[CoeffA[m, r] * s[n,k]^r, {r, 0, m}];

X[m_, t_, a_, b_] := Expand[(-1)^(m) Sum[Sum[Binomial[j, t] CoeffA[m, j] k^(2 j - t) (-1)^j, {j, t, m}], {k, a, b-1}]];

H[m_, t_, k_] := Sum[Binomial[j, t] * CoeffA[m, j] * (-1)^j / (2 j - t + 1) * Binomial[2 j - t + 1, k]*BernoulliB[2 j - t + 1 - k], {j, t, m}];

P[m_, n_, a_, b_] := Expand[Sum[L[m, n, k], {k, a, b-1}]];

End[ ]

EndPackage[ ]


(* ::Code:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::Code:: *)
(**)
