{
  "More Information" -> {
    "Many standard Mathematica operations have been defined to operate naturally on DataRegions.",
    "Map[f,d] produces a DataRegion with f mapped over the data part of the DataRegion d.",
    "MapThread[f,{d1,d2,...}] produces a DataRegion with f mapped over the data parts of the DataRegions d1, d2, ... in the same way as for MapThread on lists."
  },

  "Basic Examples" -> {
    "d = ToDataRegion[{{1,2,3},{4,5,6}}, {10, 20, 30}, {0.1, 0.2, 0.3}]",
    "ToList[d]",
    "d2 = Map[Sin,d]",
    "ToList[d2]",
    "d3 = MapThread[f,{d,d2}]",
    "ToList[d3]"
    },
  "See Also" -> 
    Names["DataRegion`*"],
  "More About" -> {"nrmma", "Data Representations"}
}
