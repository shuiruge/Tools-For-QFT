(* ::Package:: *)

(* This is a package that contains the functions (modules) used by function "actOn".
	This function contains variables "Operator, State", where the first denotes the operator acting and the second denotes the state (described by a list) acted on.
*)


<<ToolsForQFT`Packages`Basics`FundamentalQNumber`;
(* Load the "FundamentalQNumber.m" package, which contains the:
"fundamentalQNumberQ";
"bosonOrFermion"
, which is needed here. *)


pureActOn[Operator_, State_] := Module[{result, Integrand, Variable, TheFirst, TheResidual, initialList, finalList, i1},
	If[fundamentalQNumberQ[Operator] == False,
		result = "This operator is neither a creation operator nor an annihilation operator!"];
	If[ToString[Head@State] != "ket",
		(* The state is not a "pure ket". *)
		(* Then, simplify the 'State': *)
		If[ToString[Head@State] == "Integrate",
			(* $a . \{ \int \Ket{\Phi_p} dp \}$ -> $\int dp \{ a . \Ket{\Phi_p} \}$: *)
			result = Integrate[pureActOn[Operator, State/.{Integrate[Integrand_,Variable_] -> Integrand}],
							State/.{Integrate[Integrand_,Variable_] -> Variable}]];
		If[ToString[Head@State] == "Plus",
			(* $a . \{ \Ket{\Phi_1} + \Ket{\Phi_2} \}$ -> $a . \Ket{\Phi_1} + a . \Ket{\Phi_2}$: *)
			result = Plus[pureActOn[Operator, State/.{Plus[TheFirst_, TheResidual_] -> TheFirst}],
						pureActOn[Operator, State/.{Plus[TheFirst_, TheResidual_] -> TheResidual}]]];
		If[ToString[Head@State] == "Times",
			(* $a . \{ c*\Ket{\Phi} \}$ -> $c*\{ a . \Ket{\Phi} \}$, where $a$ is a q-number and $c$ is DEFINITLY a c-number: *)
			If[ketQ[State/.{Times[TheFirst_, TheResidual_] -> TheFirst}] == True && ketQ[State/.{Times[TheFirst_, TheResidual_] -> TheResidual}] == False,
				result = Times[pureActOn[Operator, State/.{Times[TheFirst_, TheResidual_] -> TheFirst}],
							State/.{Times[TheFirst_, TheResidual_] -> TheResidual}]];
			If[ketQ[State/.{Times[TheFirst_, TheResidual_] -> TheFirst}] == False && ketQ[State/.{Times[TheFirst_, TheResidual_] -> TheResidual}] == True,
				result = Times[State/.{Times[TheFirst_, TheResidual_] -> TheFirst},
							pureActOn[Operator, State/.{Times[TheFirst_, TheResidual_] -> TheResidual}]]]
		]
		(* Can the Head@State be "**"??? *)
	];
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
					];]]]
	];
	Return[result];
];


(* Blows are some sub-functions used in function "pureActOn". *)


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
		resultAsString = StringInsert[
							StringInsert[
								StringTake[functionAsString, {i1, StringLength@functionAsString - 1}],
							"{", 1],
						"}", -1];
		result = ToExpression@resultAsString
	];
	Return[result];
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


(* Test whether "State" is a ket, maybe mixed, such as "c*ket[{}]", where "c" is a c-number, or not. *)
(* The mechanism of this is to see whether there's "ket" as a "Head" in "State", or not. *)

ketQ[State_] := Module[{result = False, i1 = 1},
	If[ToString[Head@State] == "ket",
		result = True];
	If[ToString[Head@State] != "ket" && ToString@variableAsList[State] == "No variable!",
		result = False];
	If[ToString[Head@State] != "ket" && ToString@variableAsList[State] != "No variable!",
		While[i1 <= Length@variableAsList[State] &&  result == False,
			result = ketQ[pickOutVariable[State, i1]];
			i1++]];
	Return[result];
];
