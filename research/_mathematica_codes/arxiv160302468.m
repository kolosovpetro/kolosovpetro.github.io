(* ::Package:: *)

BeginPackage["CoeffA`"]

CoeffA::usage= "CoeffA[n,k] produces the k-th coefficient A in n-th row."

U::usage= "U[n,k] := k(n-k). "

Q::usage= "Q[r_,n_] := Sum[k^r(n-k)^r,{k, 0, n-1}];"

ConvolutionTable::usage= "ConvolutionTable[n] gives the n-th row triagle of Q[r_,n_] that is convolution of r-th power."

TriangleQ::usage= "TriangleQ[n] gives n rows of triangle filled by Q[n,k], k<=n. "
	
TriangleCoeffA::usage= "TriangleCoeffA[j] prints a tringular array of coefficients CoeffA[n,k] consisting of j-rows."
	
ColumnTriangleA::usage= "ColumnTriangleA[j, t] prints t terms of j-th column of TriangleCoeffA[h]."

DiagonalTriangleA::usage= "DiagonalTriangleA[r,t] prints t items of r-th diagonal of TriangleCoeffA[h]."

CenteredColumnTriangleA::usage= "CenteredColumnTriangleA[r,t] prints t items of r-th centered column of TriangleCoeffA[h]."

L::usage= "L[m, n, k] gives an integer value of polynomial L with repsect to m, n, k. For m=1 see triangle https://oeis.org/A287326."

TriangleL::usage= "TriangleL[m_, t_] generates numerical triangle of t-rows filled by L[m, n, k]."

GeneralizedTriangleL::usage= "GeneralizedTriangleL[m, t, radius] gives a generalized triangle TriangleL[m, t] with rows from -radius to +rad."

RowGeneralizedTriangleL::usage= "RowGeneralizedTriangleL[m, t, radius] gives an t-th row of TriangleL[m, t] form -radius to +rad."

RowSumGeneralizedTriangleL::usage= "RowSumGeneralizedTriangleL[m_, n_, radius_] gives the sum of n-th row of  GeneralizedTriangleL[m, t, radius] with radius_."

RowTriangleL::usage= "RowTriangleL[m, n] prints n-th row of  TriangleL[m, t], t >= n."

ColumnTriangleL::usage= "ColumnTriangleL[m, k, t] prints the t terms of k-th column of TriangleL[m, n], n >= k."

ColumnSumTriangleL::usage= "ColumnSumTriangleL[m, r, s] gives the partial sums of ColumnTriangleL[m, k, t] over k from 0 to s."

ClosedFormColumnSumL::usage= "ClosedFormColumnSumL[m_, k_, max_] gives the value of closed form of column sum of Triangle L ."

CenteredColumnTriangleL::usage= "CenteredColumnTriangleL[m, r, t] gives t terms of r-th centered column of TriangleL[m, t]."

OddPowerIdentity::usage= "OddPowerIdentity[n_, m_] gives integer n^(2m+1), by means of sum of L[m,n,k] for natural m,n."

ClosedFormOddPowerIdentity::usage= "ClosedFormOddPowerIdentity[m_, T_] gives closed form of identity for any particular natural T."

ClosedFormOddPowerIdentityByRadius::usage= "ClosedFormOddPowerIdentityByRadius[m_, T_, radius_] gives closed form of identity for any particular natural T from -radius to +radius."

ClosedFormOddPowerIdentityList::usage= "ClosedFormOddPowerIdentityList[m, n, t] generates a list of ClosedFormOddPowerIdentity[m, T] containing t terms."

ClosedFormCoefficient::usage= "ClosedFormCoefficient[m, l, t] prints the coefficient of l-th power in ClosedFormOddPowerIdentity[m_, T_] for any given T. Note that l =< t."

ClosedFormCoefficientList::usage= "ClosedFormCoefficientList[m, r] prints r lines of ClosedFormCoefficient[m, l, t]."

ClosedFormCoefficientColumn::usage= "ClosedFormCoefficientColumn[m, t, r] gives column of ClosedFormCoefficientList[m, r] of power t =< m."

BinomialTriangle::usage= "BinomialTriangle[n_, s_] prints binomial triangle of s rows for any integer n."

Numerical::usage= "Numerical[n, k] gives terms of numerical expansion of monomials."

NumericalTriangle::usage= "NumericalTriangle[n, k] gives triangle of Numerical[n, k]."

CoeffLamda::usage= "CoeffLamda[m, r, k] gives Lamda coefficients from definition 1.6."

LamdaOddPowerIdentity::usage= "LamdaOddPowerIdentity[T, s] gives an odd power identity T^(2s+1)."

LamdaOddPowerIdentityClosedForm::usage= "LamdaOddPowerIdentityClosedForm[T, l, s] gives a closed form of odd power identity T^(2s+1) for two variables T,l from."

ColumnSumClosedForm::usage= "ColumnSumClosedForm gives a closed for of column of L[m,n,k]."

Begin["`Private`"]

Unprotect[Power];

Power[0|0., 0|0.] = 1;

Protect[Power];

U[n_, k_]:=k(n-k);

Q[r_,n_] := Sum[k^r(n-k)^r,{k, 0, n-1}];

ConvolutionTable[n_]:= Column[Table[Q[r,s-r],{s,0,n},{r,0,s}], Left];

TriangleQ[n_]:= Column[Table[U[s, k], {s, 0, n}, {k, 0, s}], Left];

CoeffA[n_, k_] := 0;
CoeffA[n_, k_] := (2 k + 1)*Binomial[2 k, k]* Sum[CoeffA[n, j]*Binomial[j, 2 k + 1]*(-1)^(j - 1)/(j - k)* BernoulliB[2 j - 2 k], {j, 2 k + 1, n}] /; 0 <= k < n;
CoeffA[n_, k_] := (2 n + 1)*Binomial[2 n, n] /; k == n;

TriangleCoeffA[j_]:= Column[Table[ CoeffA[n, k], {n, 0, j}, {k, 0, n}], Left];

ColumnTriangleA[j_, t_] := Table[CoeffA[n, j], {n, j, t}];

DiagonalTriangleA[r_, t_] := Table[CoeffA[j + r, j], {j, 0, t}];

CenteredColumnTriangleA[r_, t_] := Table[CoeffA[2 n + r, n], {n, 0, t}];

L[m_, n_, k_] := Sum[CoeffA[m, r] * U[n,k]^r, {r, 0, m}];

TriangleL[m_, t_] := Column[Table[L[m, n, k], {n, 0, t}, {k, 0, n}], Center];

GeneralizedTriangleL[m_, t_, radius_] := Column[Table[L[m, n, k], {n, 0, t}, {k, -radius , n+radius}], Left];

RowGeneralizedTriangleL[m_, n_, radius_] := Table[L[m, n, s], {s, -radius, n+radius}];

RowSumGeneralizedTriangleL[m_, n_, radius_]:= Sum[L[m, n, k], {k, -radius, n+radius}];

RowTriangleL[m_, n_] := Table[L[m, n, k], {k, 0, n}];

ColumnTriangleL[m_, k_, t_] := Table[L[m, n, k], {n, k, t+k}];

ColumnSumTriangleL[m_, r_, s_]:= Table[Sum[L[m, k, r], {k, r, t}], {t, r, s+r}];

ClosedFormColumnSumL[m_, k_, max_]:=Sum[L[m, s, k], {s, k, max}];

CenteredColumnTriangleL[m_, r_, t_] := Table[L[m, 2 n + r, n], {n, 0, t}];

OddPowerIdentity[n_, m_] := Sum[CoeffA[m,r]*Sum[k^r(n-k)^r, {k, 0, n}], {r, 0, m}];

ClosedFormOddPowerIdentity[m_, n_, T_] := Expand[Sum[L[m, n, k], {k, 0, T}]];

ClosedFormOddPowerIdentityByRadius[m_, T_, radius_] := Expand[Sum[L[m, T, k], {k, -radius, T+radius}]];

ClosedFormOddPowerIdentityList[m_, n_, t_] := Column[Table[ClosedFormOddPowerIdentity[m, n, f], {f, 0, t}], Left];

ClosedFormCoefficient[m_, l_, t_] := (-1)^m Sum[Sum[Binomial[j, t] CoeffA[m, j] k^(2 j - t) (-1)^j, {j, t, m}], {k, 0, l}];

ClosedFormCoefficientList[m_, r_] := Column[Table[ClosedFormCoefficient[m, l, t], {l, 0, r}, {t, 0, m}], Left];

ClosedFormCoefficientColumn[m_, t_, r_] := Table[ClosedFormCoefficient[m, l, t], {l, 0, r}];

BinomialTriangle[s_, t_] := Column[Table[Binomial[n, k]*s^k, {n, 0, t}, {k, 0, n}], Center];

Numerical[n_, k_] := 0;
Numerical[n_, k_] := 1 /; k == 0 || k == n;
Numerical[n_, k_] := Sum[n^s, {s, 0, n-1}] /; 0 < k< n;

NumericalTriangle[n_, t_] := Column[Table[Numerical[n,k], {n, 0, t}, {k, 0, n}], Center];

CoeffLamda[m_, r_, k_] := Sum[Binomial[j, r] * CoeffA[m,j] * (-1)^j / (2j - r + 1) * Binomial[2j - r + 1, k] * BernoulliB[2j - r + 1 - k], {j, r, m}];

LamdaOddPowerIdentity[T_, s_] := Sum[Sum[(-1)^(2s-r) * CoeffLamda[s, r, k] * T^(k+r), {k, 0, 2s+1-r}], {r, 0, s}];

LamdaOddPowerIdentityClosedForm[T_, l_, s_] := Sum[Sum[(-1)^(2s-r) * CoeffLamda[s, r, k] * T^r * l^k, {k, 1, 2s+1-r}], {r, 0, s}];

ColumnSumClosedForm[lowlimit_, uplimit_, m_]:=Sum[L[m, uplimit, k], {k, lowlimit, uplimit}];

End[ ]

EndPackage[ ]


(* ::Code:: *)
(**)


(* ::InheritFromParent:: *)
(**)


(* ::Code:: *)
(**)
