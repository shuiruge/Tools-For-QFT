(* ::Package:: *)

(* The "variable" should be the form "{{v1, min_v1, max_v1}, {v2, min_v2, max_v2}, ...}"!!! *)


Get[path<>"Basics/BasicAlgebraicOperations/OperationsOnVariables.m"];
Get[path<>"Basics/BasicAlgebraicOperations/LnAndExp.m"];
Get[path<>"Basics/integrateAdv/IntegrateAdvanced.m"];


planeWaveTheReplaceRulesForIntegral = {Integrate[E^(I k_ x_), {x_, -Infinity, +Infinity}]-> DiracDelta[k]};

(*
integrateAdv[integrand_, variable_] := Module[{result, theFirst, theResidual},
	If[Length@variable == 1,
		If[ToString@Head@integrand == "Times",
			If[MemberQ[Level[integrand/.{Times[theFirst_ , theResidual_] -> theFirst}, {0,+Infinity}], variable[[1,1]]] == False,
				(* That is, the first part is free of "variable[[1]]". *)
				result = Times[integrand/.{Times[theFirst_ , theResidual_] -> theFirst}, integrateAdv[integrand/.{Times[theFirst_ , theResidual_] -> theResidual}, variable]]];
			If[MemberQ[Level[integrand/.{Times[theFirst_ , theResidual_] -> theResidual}, {0,+Infinity}], variable[[1,1]]] == False,
				(* That is, the residual part is free of "variable[[1]]". *)
				result = Times[integrateAdv[integrand/.{Times[theFirst_ , theResidual_] -> theFirst}, variable], integrand/.{Times[theFirst_ , theResidual_] -> theResidual}]]];
		If[ToString@Head@integrand != "Times",
			result = Integrate[integrand, variable[[1]]]/.replaceRulesForIntegral]
	];
	
	Return@result;
]
*)


integrateAdv[integrand_, variableList_, rulesAdded_] := Module[{result, result1, var = variableList[[1,1]], integrandList},
	If[Length@variableList === 1,
		integrandList = seperateVariable[integrand, var];
		(* If the integrand contains only the "var" as variable, such as,
		A[var]: *)
		If[Length@integrandList[[2]] === 0,
			result = Integrate[integrand, variableList[[1]]]/.rulesAdded,
		(* If not, i.e., if the integrand is a mixed one of different variables,
		such as, B[x]*A[var]: *)
		(* Then, seperate it into the part containing the "var" and the part without
		it. By this, the integrate can be reduced to the above case, multiplied by
		the residual which is the part without the "var". *)
			result = Times[integrateAdv[integrandList[[1]], variableList, rulesAdded],
						integrandList[[2]]]]
	];
	If[Length@variableList =!= 1,
		result = integrateAdv[integrateAdv[integrand, Delete[variableList,1], rulesAdded],
						{variableList[[1]]}, rulesAdded]];
	Return@result;
];





(* This function seperate the expression as a product of sub-expressions,
with only one of which contains the variable "var".
Input the expression which is a production of some sub-expressions and
the variable needed to be picked out, with output being a list
"{free of 'var' part, contain 'var' part}". *)

(* Such as: input "A[x]*B[y]*C[z]" and "x", output "{A[x], B[y]*C[z]}". *)


seperateVariable[expr_, var_] := Module[{result, subExprList, i1 = 1, freeOfVarList = {}, containVarList = {}, freeOfVar, containVar},
	If[Head@expr =!= Times && Head@expr =!= Power,
		(* This function only deals with productions. If not a production,
		return the expr itself. *)
		result = {expr, {}},
		If[Head@expr === Power && Head[Level[expr, 1][[2]]] =!= Plus,
			(* Such as, "Exp[I k x]", which is a 'pure' one itself. *)
			(* Or, such as, "Exp[(m + n)^x]", which is "Power[E, Plus[m,n], x]". *)
			result = {expr, {}},
			(* If not the above two cases, we regard the expr as a mixed one,
			such as, "A[x]*B[y]*C[z]". *)
			subExprList = Level[ExpandAll@ln[expr], 1];
			While[i1 <= Length@subExprList,
				If[freeOfVariableQ[subExprList[[i1]], var] == True,
					AppendTo[freeOfVarList, subExprList[[i1]]]];
				If[freeOfVariableQ[subExprList[[i1]], var] == False,
					AppendTo[containVarList, subExprList[[i1]]]];
				i1++];
			freeOfVar = exp@Sum[freeOfVarList[[j1]], {j1, 1, Length@freeOfVarList}];
			containVar = exp@Sum[containVarList[[j2]], {j2, 1, Length@containVarList}];
			result = {containVar, freeOfVar}
		]];
	Return@result;
];


freeOfVariableQ[expr_, var_] :=
	Not@MemberQ[Level[expr, {0, +Infinity}],
		var];


ln[expr_] := Module[{result},
	If[Head@expr =!= Power && Head@expr =!= Times,
		result = Log[expr]];
	If[Head@expr === Power,
		result = Log[Level[expr, 1][[1]]]*(Apply[Power, Delete[Level[expr, 1], 1]])];
	If[Head@expr === Times,
		result = Apply[Plus, Map[ln, Level[expr, 1]]]];
	Return@result;
];


exp[expr_] := Module[{result},
	If[Head@expr =!= Plus,
		result = Exp[expr]];
	If[Head@expr === Plus,
		result = Apply[Times, Map[exp, Level[expr, 1]]]];
	Return@result;
];
