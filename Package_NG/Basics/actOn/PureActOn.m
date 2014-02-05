(* ::Package:: *)

(* This is a package that contains the functions (modules) used by function "actOn".
	This function contains variables "Operator, State", where the first denotes the operator acting and the second denotes the state (described by a list) acted on.
*)


Get[path<>"Basics/BasicAlgebraicOperations/FundamentalQNumber.m"];
(* Load the "FundamentalQNumber.m" package, which contains the:
"fundamentalQNumberQ";
"bosonOrFermion"
, which is needed here. *)


pureActOn[Operator_, State_] := Module[{result, Integrand, Variable, TheFirst, TheResidual, initialList, finalList, i1},
	If[Head@State =!= ket,
		(* The state is not a "pure ket". *)
		(* Then, simplify the 'State': *)
		If[Head@State === Integrate,
			(* $a . \{ \int \Ket{\Phi_p} dp \}$ -> $\int dp \{ a . \Ket{\Phi_p} \}$: *)
			result = dealWithIntegrateForPureActOn[Operator, State]];
		If[Head@State === integrateAdv,
			(* $a . \{ \int \Ket{\Phi_p} dp \}$ -> $\int dp \{ a . \Ket{\Phi_p} \}$: *)
			result = dealWithIntegrateAdvForPureActOn[Operator, State]];
		If[Head@State === Plus,
			(* $a . \{ \Ket{\Phi_1} + \Ket{\Phi_2} \}$ -> $a . \Ket{\Phi_1} + a . \Ket{\Phi_2}$: *)
			result = dealWithPlusForPureActOn[Operator, State]];
		If[Head@State === Times,
			(* $a . \{ c*\Ket{\Phi} \}$ -> $c*\{ a . \Ket{\Phi} \}$, where $a$ is a q-number and $c$ is DEFINITLY a c-number: *)
			result = dealWithTimesForPureActOn[Operator, State]];
		If[Head@State === NonCommutativeMultiply,
			result = dealWithNonCommutativeMultiplyForPureActOn[Operator, State]],
	(* Otherwise, if the state itself is just "ket[{...}]": *)
		If[Head@Operator === dagger,
			(* Means that it is a creation operator. *)
			(* Thus, by equ.(4.2.1) in S.W.'s QTF: *)
			initialList = pickOutVariable[State, 1];
			If[Length[initialList] == 0, (* "State" is vacuum. *)
				finalList = {pickOutVariable[Operator,1]}];
			If[Length[initialList] != 0, (* "State" is not vaccum. *)
				finalList = AppendTo[initialList, pickOutVariable[Operator,1]]];
			result = ket[finalList]];
		If[Head@Operator =!= dagger,
			(* Means that it is an annihilation operator. *)
			(* Thus, by equ.(4.2.3) in S.W.'s QTF: *)
			initialList = pickOutVariable[State, 1];
			If[Length[initialList] == 0 (* The "State" is vaccum. *),
				result = 0];
			If[Length[initialList] > 0 (* The "State" is not vaccum. *),
				result = 0;
				For[i1 = 1, i1 <= Length[initialList], i1++,
					If[Head@Operator === Head[initialList[[i1]]],
						(* To make sure that the "i1"th particle in the ket is just
							the SAME kind of particle that we want to annihilate. *)
						result = result + Product[DiracDelta[pickOutVariable[Operator,i2] - pickOutVariable[initialList[[i1]],i2]], {i2,1,3}]*ket[Delete[initialList, i1]]
					];]]]
	];
	Return[result];
];


dealWithIntegrateForPureActOn[Operator_, State_] :=
	Module[{result, Integrand, Integrand0, Variables, Variables0},
		Integrand = State/.{Integrate[Integrand0_, Variables0__] -> Integrand0};
		Variables = State/.{Integrate[Integrand0_, Variables0__] -> Variables0};
		result = Integrate[pureActOn[Operator, Integrand], Variables];
		Return@result;];


dealWithIntegrateAdvForPureActOn[Operator_, State_] :=
	Module[{result, Integrand, Integrand0, VariablesList, VariablesList0, Rules, Rules0},
		Integrand = State/.{integrateAdv[Integrand0_, VariablesList0_, Rules0] -> Integrand0};
		VariablesList = State/.{integrateAdv[Integrand0_, VariablesList0_, Rules0] -> VariablesList0};
		Rules = State/.{integrateAdv[Integrand0_, VariablesList0_, Rules0] -> Rules0}
		result = integrateAdv[pureActOn[Operator, Integrand], VariablesList, Rules];
		Return@result;];


dealWithPlusForPureActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = State/.{Plus[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = State/.{Plus[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = Plus[pureActOn[Operator, TheFirst],
					pureActOn[Operator, TheResidual]];
		Return@result;];


dealWithTimesForPureActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = State/.{Times[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = State/.{Times[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = If[ketMemberQ[TheFirst] == True,
					Times[pureActOn[Operator, TheFirst],
						TheResidual],
					Times[TheFirst,
						pureActOn[Operator, TheResidual]]];
		Return@result;];


dealWithNonCommutativeMultiplyForPureActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = State/.{NonCommutativeMultiply[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = State/.{NonCommutativeMultiply[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = If[ketMemberQ[TheFirst] == True,
					Times[pureActOn[Operator, TheFirst],
						TheResidual],
					Times[TheFirst,
						pureActOn[Operator, TheResidual]]];
		Return@result;];


(* Blows are some sub-functions used in function "pureActOn". *)


(* Function "variableAsList[]" and "pickOutVariable[]" within 
package "OperationsOnVariables.m" are used here. *)

Get[path<>"Basics/BasicAlgebraicOperations/OperationsOnVariables.m"];


(* Test whether "State" is a ket, maybe mixed, such as "c*ket[{}]", where "c" is a c-number, or not. *)
(* The mechanism of this is to see whether there's "ket" as a "Head" in "State", or not. *)

ketMemberQ[State_] :=
	Module[{result = False, levelList, i1 = 1},
		levelList = Level[State, {0, +Infinity}];
		While[i1 <= Length@levelList && result != True,
			If[Head[levelList[[i1]]] === ket,
				result = True];
			i1++];
		Return@result;
	];
