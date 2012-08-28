Module[{packages},

packages = Table[{"Title" -> ToString[ps[[1]]], "Functions" -> ToString /@ ps[[2]]}, {ps, Select[$PackageSymbols, #[[2]] =!= {} &]}];

{
 "Application" -> "nrmma",
 "Package" -> "Overview",
 "Title" -> "NRMMA Overview",
 "Summary" -> 
   "Overview of all functions and symbols provided by nrmma",
 "Description" -> "This page gives an overview of all the functions and symbols provided by nrmma.",
 "Keywords" -> {"nrmma", "NRMMA", "Overview"},
 "Label" -> "NRMMA Application",
 "Synonyms" -> {"NRMMAOverview", "nrmmaOverview"},
 "URL" -> "http://bitbucket.org/ianhinder/nrmma" ,
 "Packages" -> packages
}
]
