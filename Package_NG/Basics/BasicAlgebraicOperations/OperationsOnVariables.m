(* ::Package:: *)

(*

(* The most stupid way, but it really works! *)

variableAsList[function_] := Module[{result, resultAsString, functionAsString, atom = 1, i1 = 1},
	functionAsString = ToString@FullForm[function];
	While[atom != "[",
		If[i1 == StringLength@functionAsString,
			(* That is, there's no "[...]" in function. *)
			result = "No variable!";
			Break[]];
		atom = StringTake[functionAsString, {i1}];
		i1++];
	If[i1 != StringLength@functionAsString,
		(* Not the case where there's no "[...]" in function. *)
		resultAsString = StringInsert[
							StringInsert[
								StringTake[functionAsString, {i1, StringLength@functionAsString - 1}],
							"{", 1],
						"}", -1];
		result = ToExpression@resultAsString
	];
	Return[result];
];

*)


(* Another way: *)

variableAsList[function_] := Module[{result},
	result = Level[function, 1];
	Return@result;
];


(* Input an function, NOT ONLY WITH ITS HEAD, such as "f[x1,x2]",
	as well as the position of the variable you want to pick out,
	such as "2", then output the variable of the function at that
	position, that is, "x2". *)


pickOutVariable[function_, position_] := Module[{result},
	If[ToString@variableAsList[function] == "No variable!",
		result = "No variable at all!"];
	If[ToString@variableAsList[function] != "No variable!",
		If[position > Length@variableAsList[function],
			result = "Not that many variables!"];
		If[position <= Length@variableAsList[function],
			result = variableAsList[function][[position]]]
	];
	Return@result;
];


(* To see whether the "function" is free of the "variable" or not.
If yes, return "True", else "False". *)


freeOfVariableQ[function_, variable_] := Module[{result},
	result = Not@MemberQ[Level[function, {0, +Infinity}], variable];
	Return@result;
];
