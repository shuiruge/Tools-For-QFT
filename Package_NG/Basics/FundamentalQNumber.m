(* ::Package:: *)

(* This package contains the functions:
	"fundamentalQNumberQ";
	"bosonOrFermion". *)


(* This function is used to judge whether an operator is a fundamental q-number, i.e., whether it's contained in the list "FundamentalQNumbers", which're usually the creation & annihilation operators, or not.
	If yes, return True, else return False. *)

(* Note that, the "Operator" is something like "a[p1, p2, p3]", rather than only "a"!!!
	The reason why setting so is that this will make the function "actOn" much simpler!! *)


fundamentalQNumberQ[Operator_] := Module[{result = False, head, SubOperator, i1 = 1},
	If[ToString[Head@Operator] == "dagger",
		result = fundamentalQNumberQ[Operator/.{dagger[SubOperator_] -> SubOperator}]
	];
	If[ToString[Head@Operator] != "dagger",
		head = Head@Operator;
		While[i1 <= Length[FundamentalQNumbers] && result == False,
			If[head == FundamentalQNumbers[[i1,1]],
				result = True];
			i1++;]];
	Return@result;
]


(* This function is used to tell whether an operator is a boson one or a fermion one.
	If boson one, return "boson", else return "fermion". *)

(* Needed to be re-written!!! *)


bosonOrFermion[Operator_] := Module[{result = 0, i1 = 1},
	If[fundamentalQNumberQ[Operator] == False,
		Print["It's not been declared!"]];
	If[fundamentalQNumberQ[Operator] == True,
		While[i1 <= Length[FundamentalQNumber] && result = 0,
			If[Operator == FundamentalQNumber[[i1,1]],
				result = FundamentalQNumber[[i1,2]]];
			i1++;]];
	Return@result;
];
