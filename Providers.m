
BeginPackage["Providers`", {"Error`"}];

CallProvidedFunction;
ProviderPreferences::usage = "ProviderPreferences[f] gives a list of providers for feature f which determines the preference for data from these providers.  This can be reset by the user, either globally or within a Block.";

Begin["`Private`"];

ProviderPreferences[_] := {};

CallProvidedFunction[base_String, fn_String, args_List, method_:Automatic] :=
  Module[
    {providerHaveFns, providers, provider, have, posns, f, numProviders, providersWithData},

    (* Print["CallProvidedFunction["<>StringJoin[Riffle[ToString[#,InputForm]& /@ {base, fn, args, method},","]]<>"]"]; *)

    providerHaveFns = Names["*`"<>base<>"`HaveData"];
    providers = Map[First[StringSplit[#,"`"]] &, providerHaveFns];

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
      CallProvidedFunction::nodata = "No data for `1`";
      ErrorMessage[CallProvidedFunction::nodata, base<>"`"<>fn<>"["<>
                   StringJoin[Riffle[ToString[#,InputForm]&/@args,","]]<>"]"],

      numProviders > 1,
      providersWithData = Extract[providers,posns];
      Module[
        {choices = Select[ProviderPreferences[base], MemberQ[providersWithData,#]&]},
        If[choices === {},
           CallProvidedFunction::mulsrc = "Multiple data sources for `1` (`2`) and ProviderPreferences[`3`] does not specify which is preferred.  Please set ProviderPreferences[`3`] to a list of providers for `1` indicating the order in which they are preferred.";
           ErrorMessage[CallProvidedFunction::mulsrc, base, StringJoin[Riffle[providersWithData,", "]], ToString[base,InputForm]],
           (* else *)
           First[choices]]]];
    
    If[!MemberQ[providers, provider],
       Error[base<>": Provider "<>ToString[provider,InputForm]<>" not found"]];

    If[Names[provider<>"`"<>base<>"`"<>fn] === {},
       Error["Function "<>provider<>"`"<>base<>"`"<>fn<>" not found"]];

    f = ToExpression[provider<>"`"<>base<>"`"<>fn];
    (* Print["calling ", f, " with ", {args}]; *)
    f@@args];

End[];

EndPackage[];
