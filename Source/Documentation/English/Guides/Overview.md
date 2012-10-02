Module[{packages, collect, sections, sections2},

collect = {"Numerical Relativity" -> {"DataAnalysis", "InitialData", "Kicks", "NRExport", "Waveforms"}};

sections = Select[$PackageSymbols, #[[2]] =!= {} &];


newSyms = {collect[[1,1]] -> Flatten[(collect[[1,2]] /. sections),1]};
sections2 = Join[DeleteCases[sections, _?(MemberQ[collect[[1,2]], #] &) -> _], newSyms];

packages = Table[{"Title" -> ToString[ps[[1]]], "Functions" -> ToString /@ ps[[2]]}, {ps, sections2}];

{
 "Application" -> "SimulationTools",
 "Package" -> "Overview",
 "Title" -> "SimulationTools Overview",
 "Summary" -> 
   "Overview of all functions and symbols provided by SimulationTools",
 "Description" -> "This page gives an overview of all the functions and symbols provided by SimulationTools.",
 "Keywords" -> {"SimulationTools", "SimulationTools", "Overview"},
 "Label" -> "SimulationTools Application",
 "Synonyms" -> {"SimulationToolsOverview", "SimulationToolsOverview"},
 "URL" -> "http://bitbucket.org/ianhinder/SimulationTools" ,
 "Packages" -> packages
}
]
