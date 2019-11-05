(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: MarkovRandomFields     *)
(* :Context: MarkovRandomFields`  *)
(* :Author: kthierbach            *)
(* :Date: 15/04/15              *)

(* :Package Version: 1.0       *)
(* :Mathematica Version:       *)
(* :Copyright: (c) 2015 kthierbach *)
(* :Keywords:                  *)
(* :Discussion:                *)

BeginPackage["MarkovRandomFields`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]

(* Exported symbols added here with SymbolName::usage *)
GMRFEstimateParameters::usage="GMRFEstimateParameters[optimizationMethod, graphStructure, empiricalCovariance, initialization] estimates the parameters specified in graphStructure.
GMRFEstimateParameters[\"Unconstrainded\", graphStructure, empiricalCovariance, initialization] optimizes the unconstrained problem with suitable methods, see FindMinimum[].
GMRFEstimateParameters[\"Constrained\", graphStructure, empiricalCovariance, initialization] optimizes the constrained problem with suitable methods, see FindMinimum[].
GMRFEstimateParameters[\"GradientDescent\", graphStructure, empiricalCovariance, initialization] optimizes the unconstrained problem with gradient descent, see options Iterations, StepSize, ReturnAll."
  Tolerance::usage="Tolerance is an option for GMRFEstimateParameters[], if the gradient is smaller then the specified value the minimization is stopped and the current value is returned."
  MaxStepSize::usage="MaxStepSize"
  StepMonitor::usage="StepMonitor"
  eAlpha::usage="eAlpha the alpha parameter for the backtracking line search, see -Boyd, Convex Optimization- . Specifies the maximal decrease of the objective function per step (in percent)."
  eBeta::usage="eBeta the beta parameter for the backtracking line search, see -Boyd, Convex Optimization- . A high parameter value corresponds to a fine-grained search but might be expensive if the objective function is computationally expensive."

GMRFConditionalExpectation::usage="GMRFConditionalExpectation[sample, mean , informationMatrix] returns the expectated values for the values in sample specified with Missing[]."
GMRFLikelihood::usage="GMRFLikelihood[sample, mean, informationMatrix] returns the likelihood for the all given values in sample other than Missing[]."

Begin["`Private`"] (* Begin Private Context *)
Clear[GMRFEstimateParameters]
GMRFEstimateParameters::nonum = nonum;
GMRFEstimateParameters::wint="The option '`1`' has to be in the interval `2`."
GMRFEstimateParameters::invmeth="Unknown method  `1`. Valid methods are, 'SteepestDescent', 'Newton', 'BFGS'";
Options[GMRFEstimateParameters] := Join[Options[FindMinimum], {StepMonitor->None,MaxStepSize->1., Iterations -> 100, Tolerance->10^-6, ReturnAll -> False, eAlpha->.01, eBeta->.8}];
GMRFEstimateParameters["Constrained", graphStructure_SparseArray, empiricalCovariance_?MatrixQ, initial_SparseArray, opts : OptionsPattern[]] := Block[
  {optsFM, variables = Table[Unique["i$"], {i, First@Dimensions@graphStructure}, {j, Last@Dimensions@graphStructure}], objective, constraints, init, estimation},
  optsFM=Sequence@@FilterRules[{opts},First/@Options[FindMinimum]];

  (*implementation*)
  objective[Map[Pattern[#, _?NumberQ] &, variables, {2}]] := -Log[Det[variables]] + Tr[variables.empiricalCovariance];
  constraints = Map[# == 0 &, DeleteCases[Flatten[-((graphStructure - 1)*variables)], 0]];
  init = Flatten[{variables, Normal@initial}, {3, 2}];
  estimation=Check[FindMinimum[{objective[variables], constraints}, init, Evaluate[optsFM]], $Failed, {FindMaximum::nrnum, FindMinimum::ucmtd}];
  If[SameQ[estimation, $Failed],
    $Failed,
    SparseArray[variables/.estimation[[2]]]
  ]
]
GMRFEstimateParameters[method_String, graphStructure_SparseArray, empiricalCovariance_?MatrixQ, initial_SparseArray, opts : OptionsPattern[]]:=Block[
  {p, optsFM, nonZeroPositions, nonZeroX, nonZeroY, E1, E2, TE1, TE2, Kx, objective, gradient, hessian, initialization, maxStep, everything, iterations, tolerance, alpha, beta, func, param, termination, return},

  If[!MemberQ[{"SteepestDescent","Newton","BFGS"},method],
    Message[GMRFEstimateParameters::invmeth, method];
    Abort[]
  ];
  {maxStep, iterations, tolerance, everything, alpha, beta} = OptionValue[{MaxStepSize, Iterations, Tolerance, ReturnAll, eAlpha, eBeta}];
  (*implementation*)
  nonZeroPositions = (LowerTriangularize@graphStructure)["NonzeroPositions"];
  nonZeroX = First /@ nonZeroPositions;
  nonZeroY = Last /@ nonZeroPositions;
  E1 = SparseArray[Table[{nonZeroX[[k]], k} -> 1, {k, Length@nonZeroX}]];
  E2 = SparseArray[Table[{nonZeroY[[k]], k} -> 1, {k, Length@nonZeroY}]];
  TE1=Transpose[E1];
  TE2=Transpose[E2];
  Kx[var:{__?NumericQ}] := E1.#.TE2 + E2.#.TE1 &@ DiagonalMatrix[var];
  objective[Km_?MatrixQ] := (-Log[Det[Km]] + Tr[Km.empiricalCovariance]) /. Complex[_, _] -> Indeterminate;
  gradient[Km_?MatrixQ] := 2*Diagonal[TE1.(empiricalCovariance - Inverse[Km]).E2];
  hessian[Km_?MatrixQ] := 2*((TE1.#.E1)*(TE2.#.E2) + (TE1.#.E2)*(TE2.#.E1)) &@Inverse[Km];
  initialization = SetPrecision[Extract[initial-(.5 DiagonalMatrix[Diagonal@initial]), nonZeroPositions],$MachinePrecision];
  Switch[method,
    "SteepestDescent",
    func[{ grad_, x_}] := Block[
      {dx, Km, t, next, obj,gr},
      dx = -grad;
      Km = Kx[x];
      t = maxStep;
      next = Kx[x + t*dx];
      obj=objective[next];
      While[!PositiveDefiniteMatrixQ[next] || SameQ[obj, Indeterminate] || obj > (objective[Km] + alpha*t*grad.dx),
        t = beta*t;
        next = Kx[x + t*dx];
        obj=objective[next];
      ];
      gr=gradient[next];
      OptionValue[StepMonitor][obj,gr];
      {gr, x + t*dx}(*returns gradient and x*)
    ];
    param={gradient[Kx@initialization], initialization};
    termination[par_]:=Norm@First@par>tolerance;
    return[par_]:={objective[#], Norm@First@par, SparseArray[#]} &@ Kx[Last@par],
    "Newton",
    func[{grad_, x_}] := Block[
      {Km, L, LT, w, dx, t, next, obj,gr},
      Km = Kx[x];
      {L, LT} = {Transpose@#, #} &@CholeskyDecomposition[hessian[Km]];
      w = LinearSolve[L, -grad];
      dx = LinearSolve[LT, w];
      t = maxStep;
      next = Kx[x + t*dx];
      obj=objective[next];
      While[!SymmetricMatrixQ[hessian[next]] || !PositiveDefiniteMatrixQ[next] || SameQ[obj, Indeterminate] || obj > (objective[Km] + alpha*t*grad.dx),
        t = beta*t;
        next = Kx[x + t*dx];
        obj=objective[next];
      ];
      gr=gradient[next];
      OptionValue[StepMonitor][obj,gr];
      {gr, x + t*dx}(*returns gradient and x*)
    ];
    param={gradient[Kx@initialization], initialization};
    termination[par_]:=Norm@First@par>tolerance;
    return[par_]:={objective[#], Norm@First@par, SparseArray[#]} &@ Kx[Last@par],
    _,
    Abort[]
  ];
  If[everything,
    return/@NestWhileList[func, param, termination, 1, iterations],
    return@NestWhile[func, param, termination, 1, iterations]
  ]
]

Clear[GMRFConditionalExpectation]
GMRFConditionalExpectation[sampleVector:{(_Missing | _?NumericQ) ..}, mu : {__?NumberQ}, informationMatrix_SparseArray] := Block[
  {dimensions, A, B, expectedValues},
  dimensions = Length@sampleVector;
  A = Map[UnitVector[dimensions, #] &,
    Flatten@Position[sampleVector, Missing[]]];
  B = Map[UnitVector[dimensions, #] &,
    Flatten@Position[sampleVector, _?NumberQ]];
  expectedValues =
      A.mu - Inverse[A.informationMatrix.Transpose[A]].(A.informationMatrix.Transpose[B]).(DeleteCases[sampleVector, Missing[]] - B.mu);
  ReplacePart[sampleVector,
    MapThread[#1 -> #2 &, {Flatten@Position[sampleVector, Missing[]],
      expectedValues}]]
]

Clear[GMRFLikelihood]
GMRFLikelihood[sampleVector:{(_Missing | _?NumericQ) ..}, mu : {__?NumberQ}, informationMatrix_SparseArray] := Block[
  {dimensions, B, sample, marginalInformationMatrix, marginalMu},
  dimensions = Length@sampleVector;
  B = SparseArray@Map[UnitVector[dimensions, #] &, Flatten@Position[sampleVector, _?NumberQ]];
  sample = DeleteCases[sampleVector, Missing[]];
  marginalInformationMatrix = B.informationMatrix.Transpose[B];
  marginalMu = B.mu;
  (1/Sqrt[(2 Pi)^dimensions*Det[Inverse@marginalInformationMatrix]])* Exp[-0.5*(sample - marginalMu).marginalInformationMatrix.(sample - marginalMu)]
]

Options[NHessian] = {Scale -> 10^-3};
NHessian[f_, x_?(VectorQ[#, NumericQ] &), opts___?OptionQ] :=
    Module[{n, h, norm, z, mat, f0}, n = Length[x];
    h = Scale /. {opts} /. Options[NHessian];
    norm = If[VectorQ[h], Outer[Times, 2 h, 2 h], 4 h^2];
    z = If[VectorQ[h], DiagonalMatrix[h], h*IdentityMatrix[n]];
    mat = ConstantArray[0., {n, n}];
    f0 = f[x];
    Do[mat[[i, j]] =
        If[i == j,(*then*).5 (f[x + 2*z[[i]]] - 2 f0 +
            f[x - 2*z[[i]]]),(*else*)
          f[x + z[[i]] + z[[j]]] - f[x + z[[i]] - z[[j]]] -
              f[x - z[[i]] + z[[j]]] + f[x - z[[i]] - z[[j]]]], {i, n}, {j, i,
      n}];
    (mat + Transpose[mat])/norm]

Options[NGradient] = {Scale -> 10^-5};
NGradient[f_, x : {__?NumericQ}, opts : OptionsPattern[]] := Block[
  {h, fx, tmp},
  h = OptionValue[Scale];
  fx = f[x];
  tmp = ConstantArray[0, Length@x];
  Table[(f[x + (ReplacePart[tmp, i -> h])] - fx)/h, {i, 1, Length@x}]
]

End[]

EndPackage[]