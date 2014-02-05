(* ::Package:: *)

(* . *)


(*
cNumberActOn[Operator_, State_] := Module[{result = Null},
	If[Not[fundamentalQNumberQ[Operator] == False && ToString[Head@Operator] != "dagger" && ToString[Head@Operator] != "Integrate" && ToString[Head@Operator] != "Plus" && ToString[Head@Operator] != "Times" && ToString[Head@Operator] != "NonCommutativeMultiply"],
		Print["Make sure that the operator is not a q-number or an algebra operator, such as Plus and Times!"],
		result = Operator*State;
		Return[result]];
];
*)


cNumberActOn[Operator_, State_] := Operator*State;
