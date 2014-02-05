(* ::Package:: *)

Get[path<>"Basics/fourierTransformAdv/rulesForFourierExpansion.m"]


replaceByFourierExpansion[exprProducted_, {quantityName_, quantityNameFourierTransformed_}] :=
	Module[{result, list},
		list = seperateMultipliedSubExprsAsList@exprProducted;
		result = Table[list[[i1]]/.rulesForFourierExpansion[quantityName, quantityNameFourierTransformed], {i1, 1, Length@list}];
		Return@result;];
