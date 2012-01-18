(* Mathematica Test File *)

(******************************* Waveforms *********************************)

Module[
	{run=FileNameJoin[{$TemporaryDirectory,"wavesim_test_mp_asc"}],
	data=Table[N@{t, Exp[-(t - 15)^2/4^2] Exp[I t]}, {t, 0, 10 Pi, 2 Pi/20}]},
	CreateDirectory[run];
	Print[run];
	Export[FileNameJoin[{run,"mp_psi4_l2_m2_r30.00.asc"}],Map[{#[[1]],Re[#[[2]]],Im[#[[2]]]} &, data],"Table"];
	Test[
		ReadPsi4[run,2,2,30]
	,
		DataTable[data,RunName -> run]
	,
	TestID->"ReadPsi4_mp_asc"]]
