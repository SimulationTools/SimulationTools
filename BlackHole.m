(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["BlackHole`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`",
  "Horizons`"
 }];

ReadBlackHoleMass::usage = "ReadBlackHoleMass[sim, i] gives the mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleIrreducibleMass::usage = "ReadBlackHoleIrreducibleMass[sim, i] gives the irreducible mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleSpin::usage = "ReadBlackHoleSpin[sim, i] gives a list of DataTables of the Cartesian coordinate components of the spin angular momentum of the ith black hole in the simulation as a function of time.";

Begin["`Private`"];

ReadBlackHoleMass[run_String, i_Integer /; i > 0] :=
  ChristodoulouMass[run, i, i-1];

ReadBlackHoleIrreducibleMass[run_String, i_Integer /; i > 0] :=
  ReadAHMass[run, i];

DocumentationBuilder`SymbolDescription["ReadBlackHoleSpin"] =
  "read spin angular momentum of a black hole";

ReadBlackHoleSpin[run_String, i_Integer /; i > 0] :=
  Table[ReadIsolatedHorizonSpin[run, i-1, dir], {dir, 1, 3}];

End[];
EndPackage[];
