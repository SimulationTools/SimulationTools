
{
 "Application" -> "SimulationTools",
 "Package" -> "DataTable and DataRegion",
 "Title" -> "Data Representations",
 "Summary" -> 
   "Representations of data in SimulationTools",
 "Description" -> 
   {"SimulationTools uses two different representations of numerical data.  The DataTable is designed for time-series data, and consists of a monotonically-increasing time coordinate and corresponding data values, which can be of any type.  The time coordinate does not have to be regularly spaced.  The DataRegion represents an N-dimensional block of data on a uniform grid.  The data must consist of real or complex numbers, or the quantities None or Missing[].  The data is stored efficiently in memory. Each point has an associated set of coordinates, and these are specified by giving the origin and spacing of the DataRegion as {ox, oy, ... oz} and {dx, dy, ..., dz}.  Many commonly-used Mathematica functions are defined on compatible DataTables and DataRegions (those with the same coordinates), for example Plus (+), Minus (-), Times (*), Divide (/), Sqrt, Power (d^a), Sin, Cos, Tan etc.  See the ", TutorialLink["SimulationTools", "DataTable"], " and ", TutorialLink["SimulationTools", "DataRegion"], " tutorials."},
 "Keywords" -> {"SimulationTools"},
 "Label" -> "SimulationTools guide",
 "Synonyms" -> {},
 "URL" -> "http://bitbucket.org/ianhinder/SimulationTools" ,
 "Packages" -> {

    GuideSection[
      "DataRepresentations",
      {"ToList" -> "Convert a DataRepresentation to a Mathematica list",
       "ToListOfData" -> "extract the data from a DataRepresentation",
       "ToListOfCoordinates" -> "extract the coordinates from a DataRepresentation",
      "CoordinateRanges" -> "get the list of minimum and maximum coordinates of a DataRegion",
      "Slab" -> "extract (possibly lower-dimensional) slabs of data from a DataRegion", 
       "NDerivative" -> "Numerically differentiate a DataRepresentation"}],

    GuideSection[
      "DataTable",
      {"DataTable" -> "representation of 1-dimensional time-series data",
       "ToDataTable" -> "convert a list of numbers or another object to a DataTable",
       "Phase" -> "Compute the phase (argument) of a complex-valued DataTable"}],

   GuideSection[
     "DataRegion",
     {"DataRegion" -> "representation of arbitrary dimensional data on an evenly spaced grid",
      "ToDataRegion" -> "convert an N-dimensional nested list of numbers to a DataRegion" (*,
      "Strip" -> "Remove a certain number of points from the outer layers of a DataRegion" *)}]},

 "Tutorials" -> {
   "DataRegion",
   "DataTable"}}
