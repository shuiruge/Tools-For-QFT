(* ::Package:: *)

(* This package is used for dividing the long summation,
if we have known that there's a long summation 'in the
first level', so as to deal with it term by term. *)

(* If not do this, the internal expression would be too
large to be remained. While by dividing, each term is
simple, and keep the algorithm a polynormial one. *)


(* Input a expression which is a long summation in the
first level, then output a list whose elements are those
first level expressions of the input. *)

(* Function "variableAsList[]" within
"OperationsOnVariables.m" is used here. *)

divideLongSummationIntoList[longSummation_] := Module[{result, theFirst, theResidual, subExpr, subLongSummation},
	If[ToString[Head@longSummation] != "Plus",
		(* There's no long summation in the first level: *)
		result = {longSummation}];
	If[ToString[Head@longSummation] == "Plus",
		result = variableAsList[longSummation]];
	Return@result;
];
