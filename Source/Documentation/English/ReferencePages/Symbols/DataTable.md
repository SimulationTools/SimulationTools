{
  "More Information" -> {
    "Many standard Mathematica operations have been defined to operate naturally on DataTables.",
    "Map[f,d] produces a DataTable with f mapped over the data part of the DataTable d.",
    "MapThread[f,{d1,d2,...}] produces a DataTable with f mapped over the data parts of the DataTables d1, d2, ... in the same way as for MapThread on lists."
  },

  "Basic Examples" -> {
    "d = ToDataTable[{{0,0.5},{0.1,0.6},{0.3,0.3}}]",
    "ToList[d]",
    "d2 = Map[Sin,d]",
    "ToList[d2]",
    "d3 = MapThread[#1 + #2 &,{d,d2}]",
    "ToList[d3]"
    },
  "See Also" -> 
    Names["DataTable`*"],
  "Tutorials" -> {"SimulationTools", "DataRepresentations"}
}
