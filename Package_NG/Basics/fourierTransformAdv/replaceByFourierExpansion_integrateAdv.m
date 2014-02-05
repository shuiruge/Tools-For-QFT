(* ::Package:: *)

Get[path<>"Basics/fourierTransformAdv/rulesForFourierExpansion_integrateAdv.m"]


replaceByFourierExpansion[exprProducted_, {quantityName_, quantityNameFourierTransformed_}, rulesForIntegrateAdv_:basicRulesForIntegrateAdv] :=
	Module[{result, list},
		list = seperateMultipliedSubExprsAsList@exprProducted;
		result = Table[list[[i1]]/.rulesForFourierExpansion[quantityName, quantityNameFourierTransformed, rulesForIntegrateAdv], {i1, 1, Length@list}];
		Return@result;];
