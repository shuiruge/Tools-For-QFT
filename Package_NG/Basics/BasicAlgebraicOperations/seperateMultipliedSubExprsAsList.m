(* ::Package:: *)

Get[path<>"Basics/BasicAlgebraicOperations/LnAndExp.m"]


seperateMultipliedSubExprsAsList[exprMultipliedTogether_] :=
    Module[{result, lnExpr},
	   If[Head@exprMultipliedTogether === Times || Head@exprMultipliedTogether === Power,
	      lnExpr = ln@exprMultipliedTogether;
	      result = exp@seperateMultipliedByInteger@Level[lnExpr, 1],
	      result = exprMultipliedTogether];
	   Return@result;
	  ];



seperateMultipliedByInteger[list_] :=
    Module[{result = list},
	   (* If only one element in the list: *)
	  If[Length@list === 1,
	     result = seperateMultipliedByInteger1[list],
	     (* If more than one element in the list: *)
	     result = Join[seperateMultipliedByInteger1[{list[[1]]}],
			   seperateMultipliedByInteger[Delete[list, 1]]]];
	   Return@result;
	  ];



(* If only one element in the list: *)
seperateMultipliedByInteger1[list_] :=
    Module[{result = list, list1},
	   If[Head@list[[1]] === Times && Length[list1 = Level[list[[1]], 1]] === 2,
	      (* Such as "2 Log[x]". *)
	      If[Head@list1[[1]] === Integer,
		 result = Table[list1[[2]], {list1[[1]]}],
		 If[Head@list1[[2]] === Integer,
		    result = Table[list1[[1]], {list1[[2]]}]
		   ]]];
	   Return@result;];

