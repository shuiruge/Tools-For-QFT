(* ::Package:: *)

(* This is a package that contains the functions (modules) used by function "actOn".
	This function contains variables "Operator, State", where the first denotes the operator acting and the second denotes the state (described by a list) acted on.
*)


<<ToolsForQFT`Packages`Basics`FundamentalQNumber`;
(* Load the "FundamentalQNumber.m" package, which contains the:
"fundamentalQNumberQ";
"bosonOrFermion"
, which is needed here. *)


pureActOn[Operator_, State_] := Module[{result, initialList, finalList, i1},
	If[fundamentalQNumberQ[Operator] == False,
		Print["This operator is neither a creation operator nor an annihilation operator!"]];
	If[ToString[Head@State] != "ket",
		Print["This state is not a ket!"]];
	If[fundamentalQNumberQ[Operator] == True && ToString[Head@State] == "ket",
		If[ToString[Head@Operator] == "dagger",
			(* Means that it is a creation operator. *)
			(* Thus, by equ.(4.2.1) in S.W.'s QTF: *)
			initialList = pickOutVariable[State, 1];
			If[Length[initialList] == 0, (* "State" is vacuum. *)
				finalList = {pickOutVariable[Operator,1]}];
			If[Length[initialList] != 0, (* "State" is not vaccum. *)
				finalList = AppendTo[initialList, pickOutVariable[Operator,1]]];
			result = ket[finalList]];
		If[ToString[Head@Operator] != "dagger",
			(* Means that it is an annihilation operator. *)
			(* Thus, by equ.(4.2.3) in S.W.'s QTF: *)
			initialList = pickOutVariable[State, 1];
			If[Length[initialList] == 0 (* The "State" is vaccum. *),
				result = 0];
			If[Length[initialList] > 0 (* The "State" is not vaccum. *),
				result = 0;
				For[i1 = 1, i1 <= Length[initialList], i1++,
					If[ToString[Head@Operator] == ToString[Head[initialList[[i1]]]],
						(* To make sure that the "i1"th particle in the ket is just
							the SAME kind of particle that we want to annihilate. *)
						result = result + Product[DiracDelta[pickOutVariable[Operator,i2] - pickOutVariable[initialList[[i1]],i2]], {i2,1,3}]*ket[Delete[initialList, i1]]
					];]]];
		Return[result]];
];


(* Blow are some sub-functions used in function "pureActOn". *)


(* Input an function, NOT ONLY WITH ITS HEAD, such as, "f[x1,x2]",
	Output all its variables in a list, that is, "{x1,x2}". *)

variableAsList[function_] := Module[{head, variable, variableList},
	head = Head@function;
	variableList = function/.{head[variable__] -> {variable}};
	Return@variableList;
];


(* Input an function, NOT ONLY WITH ITS HEAD, such as "f[x1,x2]",
	as well as the position of the variable you want to pick out,
	such as "2", then output the variable of the function at that
	position, that is, "x2". *)

pickOutVariable[function_, position_] := Module[{result},
	result = variableAsList[function][[position]];
	Return@result;
];
