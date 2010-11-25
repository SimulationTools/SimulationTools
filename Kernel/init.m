(* Mathematica Init File *)

(* Load NRMMA packages when << nrmma` is run *)

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
