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

BeginPackage["SimulationTools`Providers`",
 {
  "SimulationTools`Error`"
 }];

CallProvidedFunction;
ProviderPreferences(*::usage = "ProviderPreferences[f] gives a list of providers for feature f which determines the preference for data from these providers.  This can be reset by the user, either globally or within a Block."*);
HaveData;

Begin["`Private`"];

ProviderPreferences[_] := {};

HaveData[base_String, args__] :=
  (* TODO: refactor this and CallProvidedFunction *)
  Module[{providerHaveFns, have},
    providerHaveFns = Names["SimulationTools`*`"<>base<>"`HaveData"];
    have = Map[ToExpression[#][args] &, providerHaveFns];
    Or@@have];

CallProvidedFunction[base_String, fn_String, args_List, method_:Automatic] :=
  Module[
    {providerHaveFns, providers, provider, have, posns, f, numProviders, providersWithData},

    (* Print["CallProvidedFunction["<>StringJoin[Riffle[ToString[#,InputForm]& /@ {base, fn, args, method},","]]<>"]"]; *)

    providerHaveFns = Names["SimulationTools`*`"<>base<>"`HaveData"];
    providers = Map[StringSplit[#,"`"][[2]] &, providerHaveFns];

    (* providers = ToExpression/@providerFns; *)
    have = Map[ToExpression[#]@@args &, providerHaveFns];
    Scan[If[# =!= True && # =!= False, Error["CallProvidedFunction: Unknown result for HaveData in "<>ToString[have,InputForm]]] &, have];
    (* Print["providers = ", providers]; *)
    (* Print["have = ", have]; *)
    posns = Position[have, True];
    numProviders = Length[posns];

    provider = Which[
      numProviders === 1,
      providers[[posns[[1,1]]]],

      numProviders === 0,

      Error["No data for `1`", base<>"`"<>fn<>"["<>
                   StringJoin[Riffle[ToString[#,InputForm]&/@args,","]]<>"]"],

      numProviders > 1,
      providersWithData = Extract[providers,posns];
      Module[
        {choices = Select[ProviderPreferences[base], MemberQ[providersWithData,#]&]},
        If[choices === {},
           Error["Multiple data sources for `1` (`2`) and ProviderPreferences[`3`] does not specify which is preferred.  Please set ProviderPreferences[`3`] to a list of providers for `1` indicating the order in which they are preferred.",
                 base, StringJoin[Riffle[providersWithData,", "]], ToString[base,InputForm]],
           (* else *)
           First[choices]]]];
    
    If[!MemberQ[providers, provider],
       Error[base<>": Provider "<>ToString[provider,InputForm]<>" not found"]];

    If[Names["SimulationTools`"<>provider<>"`"<>base<>"`"<>fn] === {},
       Error["Function SimulationTools`"<>provider<>"`.`"<>base<>"`.`"<>fn<>" not found"]];

    f = ToExpression["SimulationTools`"<>provider<>"`"<>base<>"`"<>fn];
    (* Print["calling ", f, " with ", {args}]; *)
    f@@args];

End[];

EndPackage[];
