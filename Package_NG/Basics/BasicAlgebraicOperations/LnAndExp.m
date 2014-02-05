(* ::Package:: *)

(* The Log[] and Exp[] by Mathematica are not complete.
   So, here we provide advanced ones as below: *)

(* ln[] and exp[]: *)

ln[expr_] :=
    Module[{result},
	   If[Head@expr =!= Power && Head@expr =!= Times,
	      result = Log[expr]];
	   If[Head@expr === Power,
	      result = Log[Level[expr, 1][[1]]]*(Apply[Power, Delete[Level[expr, 1], 1]])];
	   If[Head@expr === Times,
	      result = Apply[Plus, Map[ln, Level[expr, 1]]]];
	   Return@result;
	  ];


(**************** Ignore me please ****************)

exp[expr_] :=
    Module[{result},
	   If[Head@expr =!= Plus,
	      result = Exp[expr]];
	   If[Head@expr === Plus,
	      result = Apply[Times, Map[exp, Level[expr, 1]]]];
	   Return@result;
	  ];
