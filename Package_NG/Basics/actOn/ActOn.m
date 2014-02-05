(* ::Package:: *)

(* We must be care of the "dagger" operation!!! *)


(* This package contain the function "actOn", and its sub-funcitons.
It's used to act a complex combination of some physical quantities
which're q-numbers on the state, such as vacuum state. *)


Get[path<>"Basics/BasicAlgebraicOperations/FundamentalQNumber.m"];
(* Load the "FundamentalQNumber.m" package, which contains the "fundamentalQNumberQ" which is needed here. *)
Get[path<>"Basics/actOn/PureActOn.m"];
Get[path<>"Basics/actOn/CNumberActOn.m"];


actOn[Operator_, State_] :=
    Module[{result, Integrand, Variable, TheFirst, TheResidual},
	   (* Test if the "Operator" is completely a PURE c-number, or c-number function: *)
	   If[pureCNumberQ[Operator] == True,
	      result = cNumberActOn[Operator, State],
		(* If not a pure c-number, then it maybe a composed one as well as maybe a pure q-number: *)
		(* Composed cases first: *)
		(* $\{ \int dp a (p) \} . \Ket{\Phi}$ -> $\int dp \{ a (p) . \Ket{\Phi} \}$: *)
	      If[Head@Operator === Integrate,
			 result = dealWithIntegrateForActOn[Operator, State]];
	      If[Head@Operator === integrateAdv,
			 result = dealWithIntegrateAdvForActOn[Operator, State]];
	      (* $\{ b(p) + c (p) \} . \Ket{\Phi}$ -> $\{ b (p) . \Ket{\Phi} \} + \{ c (p) . \Ket{\Phi} \}$: *)
	      If[Head@Operator === Plus,
			result = dealWithPlusForActOn[Operator, State]];
	      (* $\{ b(p) * c (p) \} . \Ket{\Phi}$ -> $b (p) . \{ c (p) . \Ket{\Phi} \}$: *)
	      If[Head@Operator === Times,
			result = dealWithTimesForActOn[Operator, State]];
	      (* $\{ b (p) ** c (p) \} . \Ket{\Phi}$ -> $b (p) . \{ c (p) . \Ket{\Phi} \}$: *)
	      If[Head@Operator === NonCommutativeMultiply,
			result = dealWithNonCommutativeMultiplyForActOn[Operator, State]];
		(* If not a composed one, then it shall be a pure q-number: *)
	      If[Head@Operator =!= Integrate && Head@Operator =!= Plus && Head@Operator =!= Times && Head@Operator =!= NonCommutativeMultiply,
			If[fundamentalQNumberQ[Operator] == True,
		    (* That is, the "Operator" is just the PURE creation or annihilation operators. *)
				result = pureActOn[Operator, State]]]];
	   Return@result;
	  ];


dealWithIntegrateForActOn[Operator_, State_] :=
	Module[{result, Integrand, Integrand0, Variables, Variables0},
			Integrand = Operator/.{Integrate[Integrand0_,Variables0___] -> Integrand0};
			Variables = Operator/.{Integrate[Integrand0_,Variables0___] -> Variables0};
			result = If[fundamentalQNumberMemberQ[Integrand] == True,
			     Integrate[actOn[Integrand, State],
				       Variables],
			     cNumberActOn[Operator, State]];
			Return@result;];


dealWithIntegrateAdvForActOn[Operator_, State_] :=
	Module[{result, Integrand, Integrand0, VariablesList, VariablesList0, Rules, Rules0},
			Integrand = Operator/.{integrateAdv[Integrand0_,VariablesList0_, Rules0_] -> Integrand0};
			VariablesList = Operator/.{integrateAdv[Integrand0_,VariablesList0_, Rules0_] -> VariablesList0};
			Rules = Operator/.{integrateAdv[Integrand0_,VariablesList0_, Rules0_] -> Rules0};
			result = If[fundamentalQNumberMemberQ[Integrand] == True,
						integrateAdv[actOn[Integrand, State],
									VariablesList,
									Rules],
						cNumberActOn[Operator, State]];
			Return@result;];


dealWithPlusForActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = Operator/.{Plus[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = Operator/.{Plus[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = If[fundamentalQNumberMemberQ[TheFirst] == True,
			     If[fundamentalQNumberMemberQ[TheResidual] == True,
				Plus[actOn[TheFirst, State],
				      actOn[TheResidual, State]],
				Plus[actOn[TheFirst, State],
				      TheResidual]],
			     If[fundamentalQNumberMemberQ[TheResidual] == True,
				Plus[TheFirst,
				      actOn[TheResidual, State]],
				Plus[TheFirst,
				      TheResidual]]];
		Return@result;];


dealWithTimesForActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = Operator/.{Times[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = Operator/.{Times[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = If[fundamentalQNumberMemberQ[TheFirst] == True,
					If[fundamentalQNumberMemberQ[TheResidual] == True,
						actOn[TheFirst,
							  actOn[TheResidual, State]],
						Times[actOn[TheFirst, State],
							  TheResidual]],
					If[fundamentalQNumberMemberQ[TheResidual] == True,
						Times[TheFirst,
							actOn[TheResidual, State]],
						Times[TheFirst,
							TheResidual]]];
		Return@result;];


dealWithNonCommutativeMultiplyForActOn[Operator_, State_] :=
	Module[{result, TheFirst, TheFirst0, TheResidual, TheResidual0},
		TheFirst = Operator/.{NonCommutativeMultiply[TheFirst0_, TheResidual0_] -> TheFirst0};
		TheResidual = Operator/.{NonCommutativeMultiply[TheFirst0_, TheResidual0_] -> TheResidual0};
		result = If[fundamentalQNumberMemberQ[TheFirst] == True,
					If[fundamentalQNumberMemberQ[TheResidual] == True,
						actOn[TheFirst,
						       actOn[TheResidual, State]],
						Times[actOn[TheFirst, State],
							TheResidual]],
					If[fundamentalQNumberMemberQ[TheResidual] == True,
						Times[TheFirst,
							actOn[TheResidual, State]],
						Times[TheFirst,
							TheResidual]]];
		Return@result;];


(*
pureCNumberQ[Operator_] := fundamentalQNumberQ[Operator] == False && Head@Operator =!= dagger && Head@Operator =!= Integrate && Head@Operator =!= Plus && Head@Operator =!= Times && Head@Operator =!= NonCommutativeMultiply
*)

pureCNumberQ[Operator_] := fundamentalQNumberMemberQ[Operator] == False;


(* This function output true if and only if the input expr contains the fundamental q-numbers. *)
fundamentalQNumberMemberQ[expr_] :=
	Module[{result = False, i1 = 1, list = Level[expr, {0, +Infinity}]},
		While[i1 <= Length@list && result =!= True,
			result = fundamentalQNumberQ@list[[i1]];
			i1++];
		Return@result;];
