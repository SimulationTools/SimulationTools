
BeginPackage["InitialData`", {"RunFiles`", "DataTable`", "Memo`", "Providers`"}];

ReadADMMass::usage = "ReadADMMass[run] reads the ADM mass of the spacetime from the initial data slice";

Begin["`Private`"];

ReadADMMass[run_String] :=
  CallProvidedFunction["InitialData","ReadADMMass",{run}];

End[];

EndPackage[];
