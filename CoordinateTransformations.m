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

BeginPackage["SimulationTools`CoordinateTransformations`"];

(* Exported symbols *)
RadialToTortoise::usage = "RadialToTortoise[r, M] gives the value of the Schwarzschild tortoise coordinate corresponding to the radial coordinate, r.";
TortoiseToRadial::usage = "TortoiseToRadial[rstar_, M] gives the value of the radial coordinate corresponding to the Schwarzschild tortoise coordinate, rstar.";

ThornburgToCartesian::usage = "ThornburgToCartesian[{\[Theta], \[Phi], r}, patch] gives the cartesian coordinates in terms of Thornburg 04 coordinates on the specified patch.";
CartesianToThornburg::usage = "CartesianToThornburg[{x, y, z}, patch] gives the Thornburg 04 coordinates on the specified patch in terms of cartesian coordinates.";

KerrAxialTortoise::usage = "KerrAxialTortoise[a, M, r0, rmax] returns a function representing the Kerr tortoise coordinate as a function of the radial coordinate. The arguments M, a, r0 and rmax are optional and may be omitted."

IsotropicToSchwarzschild;
IsotropicToTortoise;

Begin["`Private`"];

RadialToTortoise[r_, M_:1] := r + 2 M Log[r/(2 M)-1];
TortoiseToRadial[rstar_, M_:1] := 2 M(1+ProductLog[E^(rstar/(2 M)-1)]);
IsotropicToSchwarzschild[r_, M_] := r (1 + M/(2 r))^2;
IsotropicToTortoise[r_,M_] :=
  RadialToTortoise[IsotropicToSchwarzschild[r,M],M];

ThornburgToCartesian[{\[Theta]_, \[Phi]_, r_}, patch_Integer/;(patch>=1&&patch<=6)] := Module[{x, y, z},
  Switch[patch,
    1, x = r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = (r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = (r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];,
    2, x = -r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = (-r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = (-r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];,
    3, x = (r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = (r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];,
    4, x = (-r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = -r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = (-r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];,
    5, x = (r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = (r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];,
    6, x = (-r Tan[\[Phi]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; y = (-r Tan[\[Theta]])/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2]; z = -r/Sqrt[1+Tan[\[Theta]]^2+Tan[\[Phi]]^2];
  ];
  {x, y, z}
];

CartesianToThornburg[{x_, y_, z_}, patch_Integer/;(patch>=1&&patch<=6)] := Module[{r, \[Theta], \[Phi]},
  Which[
    patch==1||patch==2, \[Theta] = ArcTan[z/x]; \[Phi] = ArcTan[y/x]; r = Sqrt[x^2+y^2+z^2];,
    patch==3||patch==4, \[Theta] = ArcTan[z/y]; \[Phi] = ArcTan[x/y]; r = Sqrt[x^2+y^2+z^2];,
    patch==5||patch==6, \[Theta] = ArcTan[y/z]; \[Phi] = ArcTan[x/z]; r = Sqrt[x^2+y^2+z^2];
  ];
  {\[Theta], \[Phi], r}
];

KerrAxialTortoise[a_, M_:1, r0_:4, rmax_:5000] :=
  Module[{geodesicEqs, geodesicEqs2, initEqs, allEqs, ut, ur, r, t, s, rSol, tSol},
    (* The geodesic equations for a null geodesic along the polar axis
       in first order in time form. *)
    geodesicEqs = {Derivative[1][ut][s]==(2M(a^2-r[s]^2) ur[s] ut[s])/((a^2+r[s]^2) (a^2-2 M r[s]+r[s]^2)), Derivative[1][ur][s]==(-((M(a^2-r[s]^2) (a^2+r[s]^2)^2 ur[s]^2)/(a^2-2M r[s]+r[s]^2))+(a^2-r[s]^2) (a^2-2M r[s]+r[s]^2) ut[s]^2)/(a^2+r[s]^2)^3};
    geodesicEqs2 = {D[t[s],s]==ut[s], D[r[s],s]==ur[s]};

    (* The geodesic is initially moving outwards. *)
    initEqs = {ut[0]==(a^2+r0^2)/(a^2-2M r0+r0^2), ur[0]==1};

    allEqs=Join[geodesicEqs, geodesicEqs2, initEqs,{t[0]==0,r[0]==r0}];

    (* Numerically solve the geodesic equations from just outside r=2 *)
    {rSol, tSol} = {r, t} /. NDSolve[allEqs, {r, t, ur, ut}, {s, 2.01M-r0, rmax}][[1]];

	(* Return rstar[r] *)
    Function[r, tSol[r-r0]+r0]
];

End[];
EndPackage[];
