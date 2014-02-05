(* ::Package:: *)

(* This package contains the rulesForFourierExpansion[]. *)


(* This function transforms:
	1. f[x, y, z, t] -> \[Integral]\[DifferentialD]k1Pup \[DifferentialD]k2Pup \[DifferentialD]k3Pup E^(I Overscript[kPup, \[RightVector]].Overscript[x, \[RightVector]]) g[k1Pup, k2Pup, k3Pup, t];
	2.1. (f^(1,0,0,0))[x, y, z, t] -> \[Integral]\[DifferentialD]k1Pup \[DifferentialD]k2Pup \[DifferentialD]k3Pup k1Pup E^(I Overscript[kPup, \[RightVector]].Overscript[x, \[RightVector]]) g[k1Pup, k2Pup, k3Pup, t];
	2.2. (f^(0,0,0,1))[x, y, z, t] -> \[Integral]\[DifferentialD]k1Pup \[DifferentialD]k2Pup \[DifferentialD]k3Pup E^(I Overscript[kPup, \[RightVector]].Overscript[x, \[RightVector]]) \!\(
\*SubscriptBox[\(\[PartialD]\), \(t\)]\(g[k1Pup, \ k2Pup, \ k3Pup, \ t]\)\);
	2.3. And so on......
	, where the f and g as the 1st and 2nd variables of this function. *)

(* The integral function used here is the Integrate[],
	rather than the integrateAdv[]. *)


rulesForFourierExpansion[quantityName_, quantityNameFourierTransformed_, rulesForIntegrateAdv_:basicRulesForIntegrateAdv] := Module[{result, k1Pup, k2Pup, k3Pup},
	k1Pup = Unique[k1];
	k2Pup = Unique[k2];
	k3Pup = Unique[k3];
	result = {Derivative[d1_, d2_, d3_, d4_][quantityName][x,y,z,t] :> integrateAdv[Exp[I (k1Pup x+k2Pup y+k3Pup z)]*(I k1Pup)^d1*(I k2Pup)^d2*(I k3Pup)^d3*D[quantityNameFourierTransformed[k1Pup,k2Pup,k3Pup,t], {t, d4}],
																				{{k1Pup, -Infinity, +Infinity}, {k2Pup, -Infinity, +Infinity}, {k3Pup, -Infinity, +Infinity}},
																				rulesForIntegrateAdv],
				quantityName[x,y,z,t] :> integrateAdv[Exp[I (k1Pup x+k2Pup y+k3Pup z)]*quantityNameFourierTransformed[k1Pup,k2Pup,k3Pup,t],
													{{k1Pup, -Infinity, +Infinity}, {k2Pup, -Infinity, +Infinity}, {k3Pup, -Infinity, +Infinity}},
													rulesForIntegrateAdv]};
	Return@result;];
