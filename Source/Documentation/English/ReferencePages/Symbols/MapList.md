{
  "Basic Examples" -> {
    "f[{t_, a_}] := a^2-t", (* Putting a semicolon here leads to an error during documentation generation *)
    "d = ToDataTable[{{0,0.5},{0.1,0.6},{0.3,0.3}}]",
    "d2 = MapList[f,d]",
    "ToList[d2]"
    },
  "See Also" -> 
    {"DataTable", "Map", "ToList"},
  "Tutorials" -> {"SimulationTools", "DataRepresentations"}
}
