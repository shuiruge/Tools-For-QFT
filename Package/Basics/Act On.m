(* ::Package:: *)

(* This package contain the function "actOn", and its sub-funcitons.
It's used to act a complex combination of some physical quantities
which're q-numbers on the state, such as vacuum state. *)


actOn[Operator_, State_] := Module[{result},
	(* First of all, when by hands, we usually make expansion, then deal it term by term. *)
	Operator = ExpandAll[Operator];
	result = actOnAfterExpansion[Operator, State];
	Return@result;
]


actOnAfterExpansion[Operator_, State_] := Module[{result, Integrand, Variable, TheFirst, TheResidual},
	If[MemberQ[FundamentalQNumbers, Head@Operator ] == True,
		(* That is, the "Operator" is just the PURE creation or annihilation operators. *)
		result = pureActOn[Operator, State]];
	If[MemberQ[FundamentalQNumbers, Head@Operator ] == False && ToString[Head@Operator] != "Integrate" && ToString[Head@Operator] != "Plus" && ToString[Head@Operator] != "Times",
		(* That is, the "Operator" is completely a PURE c-number, or c-number function. *)
		result = cNumberActOn[Operator, State]];
	(* If the "Operator" is not a PURE one, then: *)
	If[MemberQ[FundamentalQNumbers, Head@Operator ] == False,
		(* $\{ \int dp a (p) \} . \Ket{\Phi}$ -> $\int dp \{ a (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Integrate",
			result = Integrate[actOnAfterExpansion[Operator/.{Integrate[Integrand_,Variable_] -> Integrand}, State],
							Operator/.{Integrate[Integrand_,Variable_] -> Variable}]
		];
		(* $\{ b(p) + c (p) \} . \Ket{\Phi}$ -> $\{ b (p) . \Ket{\Phi} \} + \{ c (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Plus",
			result = Plus[actOnAfterExpansion[Operator/.{Plus[TheFirst_, TheResidual_] -> TheFirst}, State],
						actOnAfterExpansion[Operator/.{Plus[TheFirst_, TheResidual_] -> TheResidual}, State]]
		];
		(* $\{ b(p) * c (p) \} . \Ket{\Phi}$ -> $b (p) . \{ c (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@Operator] == "Times",
			result = actOnAfterExpansion[Operator/.{Times[TheFirst_, TheResidual_] -> TheFirst},
						actOnAfterExpansion[Operator/.{Times[TheFirst_, TheResidual_] -> TheResidual}, State]]
		];
	];
	Return@result;
]
