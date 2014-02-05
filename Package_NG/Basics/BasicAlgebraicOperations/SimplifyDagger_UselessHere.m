(* ::Package:: *)

simplifyDagger[Operator_] := Module[{result, SubOperator, Variable. tmp1, tmp2},
	If[ToString[Head@Operator] != "dagger",
		result = Operator];
	If[ToString[Head@Operator] == "dagger",
		SubOperator = Operator/.{dagger[Variable_] -> Variable};
		If[ToString[Head@SubOperator] == "dagger",
			result = SubOperator/.{dagger[Variable_] -> Variable}
		];
		(* $\{ \int dp a (p) \} . \Ket{\Phi}$ -> $\int dp \{ a (p) . \Ket{\Phi} \}$: *)
		If[ToString[Head@SubOperator] == "Integrate",
			result = Integrate[actOn[SubOperator/.{Integrate[Integrand_,Variable_] -> Integrand}, State],
							SubOperator/.{Integrate[Integrand_,Variable_] -> Variable}]
		];
(* -------------------------------------------------- *)
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
