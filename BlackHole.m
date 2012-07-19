(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["BlackHole`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`",
  "Horizons`"
 }];

ReadBlackHoleMass::usage = "ReadBlackHoleMass[run, i] gives the mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleIrreducibleMass::usage = "ReadBlackHoleIrreducibleMass[run, i] gives the irreducible mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleSpin::usage = "ReadBlackHoleSpin[run, i] gives a list of DataTables of the Cartesian coordinate components of the spin angular momentum of the ith black hole in the simulation as a function of time.";

Begin["`Private`"];

ReadBlackHoleMass[run_String, i_Integer /; i > 0] :=
  ChristodoulouMass[run, i, i-1];

ReadBlackHoleIrreducibleMass[run_String, i_Integer /; i > 0] :=
  ReadAHMass[run, i];

ReadBlackHoleSpin[run_String, i_Integer /; i > 0] :=
  Table[ReadIsolatedHorizonSpin[run, i-1, dir], {dir, 1, 3}];

End[];
EndPackage[];
