(* Mathematica Init File *)

(* Load NRMMA packages when << nrmma` is run *)

(* Work around a bug in ImportString in Mathematica 8. Without this,
   DataRegion's definition of GetData will cause problems. *)
If[$VersionNumber == 8., ImportString["", "Table"]];

(* Work around a bug in Mathematica 8 where Unprotect doesn't work with
   ListLogPlot unless we call ListLogPlot once first. *)
If[$VersionNumber == 8., ListLogPlot[{1}]];

(* We need to modify the $Path because the packages in nrmma don't
   specify their dependencies as nrmma`DataTable`, but as just
   DataTable`.  Changing this would break existing users.  When users
   are all using nrmma as a Mathematica application, we can change
   this. *)

$Path = Prepend[$Path, FileNameDrop[FindFile["nrmma`"], -2]];

Module[{packages =
 {"Ascii1D",
  "BHCoordinates",
  "CarpetHDF5",
  "Convergence",
  "DataRegion",
  "DataTable",
  "Grids",
  "Horizons",
  "Memo",
  "NR",
  "Parameters",
  "Plotting",
  "Profile",
  "RunFiles",
  "SimView",
  "SystemStatistics",
  "Tracks"}},
Scan[Get[# <> "`"] &, packages]]
