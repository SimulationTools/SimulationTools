(* Copyright (C) 2012 Ian Hinder and Barry Wardell *)

BeginPackage["InitialData`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "Providers`",
  "RunFiles`"
 }];

ReadADMMass::usage = "ReadADMMass[run] reads the ADM mass of the spacetime from the initial data slice";

Begin["`Private`"];

ReadADMMass[run_String] :=
  CallProvidedFunction["InitialData","ReadADMMass",{run}];

End[];

EndPackage[];
