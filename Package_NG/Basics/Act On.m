(* ::Package:: *)

(* We must be care of the "dagger" operation!!! *)


(* This package contain the function "actOn", and its sub-funcitons.
It's used to act a complex combination of some physical quantities
which're q-numbers on the state, such as vacuum state. *)


<<ToolsForQFT`Packages`Basics`FundamentalQNumber`;
(* Load the "FundamentalQNumber.m" package, which contains the "fundamentalQNumberQ" which is needed here. *)


actOn[Operator_, State_] := Module[{result, SubOperator, Integrand, Variable, TheFirst, TheResidual},
	If[fundamentalQNumberQ[Operator] == True,
		(* That is, the "Operator" is just the PURE creation or annihilation operators. *)
		result = pureActOn[Operator, State]];
	If[fundamentalQNumberQ[Operator] == False && ToString[Head@Operator] != "dagger" && ToString[Head@Operator] != "Integrate" && ToString[Head@Operator] != "Plus" && ToString[Head@Operator] != "Times" && ToString[Head@Operator] != "NonCommutativeMultiply",
		(* That is, the "Operator" is completely a PURE c-number, or c-number function. *)
		result = cNumberActOn[Operator, State]];
	(* If the "Operator" is not a PURE one, then: *)
	If[fundamentalQNumberQ[Operator] == False,
		(* $\{ \int dp a (p) \} . \Ket{\Phi}$ -> $\int dp \{ a (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Integrate",
			result = Integrate[actOn[Operator/.{Integrate[Integrand_,Variable_] -> Integrand}, State],
							Operator/.{Integrate[Integrand_,Variable_] -> Variable}]
		];
		(* $\{ b(p) + c (p) \} . \Ket{\Phi}$ -> $\{ b (p) . \Ket{\Phi} \} + \{ c (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Plus",
			result = Plus[actOn[Operator/.{Plus[TheFirst_, TheResidual_] -> TheFirst}, State],
						actOn[Operator/.{Plus[TheFirst_, TheResidual_] -> TheResidual}, State]]
		];
		(* $\{ b(p) * c (p) \} . \Ket{\Phi}$ -> $b (p) . \{ c (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Times",
			result = actOn[Operator/.{Times[TheFirst_, TheResidual_] -> TheFirst},
						actOn[Operator/.{Times[TheFirst_, TheResidual_] -> TheResidual}, State]]
		];
		(* $\{ b (p) ** c (p) \} . \Ket{\Phi}$ -> $b (p) . \{ c (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "NonCommutativeMultiply",
			result = actOn[Operator/.{NonCommutativeMultiply[TheFirst_, TheResidual_] -> TheFirst},
						actOn[Operator/.{NonCommutativeMultiply[TheFirst_, TheResidual_] -> TheResidual}, State]]
		];
	];
	Return@result;
];


(* -------------------------- Remarks: ----------------------------- *)

(* Here we use the function "ToString" everywhere while judging what's the "Head" of the expr. is. The reason of this is technique *)
