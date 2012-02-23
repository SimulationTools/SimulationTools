
BeginPackage["Providers`", {"Error`"}];

CallProvidedFunction;

Begin["`Private`"];

CallProvidedFunction[base_String, fn_String, args_List, method_:Automatic] :=
  Module[
    {providerHaveFns, providers, provider, have, posns, f},

    (* Print["CallProvidedFunction["<>StringJoin[Riffle[ToString[#,InputForm]& /@ {base, fn, args, method},","]]<>"]"]; *)

    providerHaveFns = Names["*`"<>base<>"`HaveData"];
    providers = Map[First[StringSplit[#,"`"]] &, providerHaveFns];

    provider =
    If[method === Automatic,
       (* providers = ToExpression/@providerFns; *)
       have = Map[ToExpression[#]@@args &, providerHaveFns];
       Scan[If[# =!= True && # =!= False, Error["CallProvidedFunction: Unknown result for HaveData in "<>ToString[have,InputForm]]] &, have];
       (* Print["providers = ", providers]; *)
       (* Print["have = ", have]; *)
       posns = Position[have, True];
       If[Length[posns] > 1,
          Error["Multiple data sources for "<>base<>" ("<>
                ToString[providers,InputForm]<>
                "): Please select one using Method -> source, where source is one of " <>
                ToString[providers]]];

       If[Length[posns] == 0,
          CallProvidedFunction::nodata = "No data for `1`";
          ErrorMessage[CallProvidedFunction::nodata, base<>"`"<>fn<>"["<>
                       StringJoin[Riffle[ToString[#,InputForm]&/@args,","]]<>"]"]];
       providers[[posns[[1,1]]]],
       method];

    (* Print["provider = ", provider]; *)

    If[!MemberQ[providers, provider],
       Error[base<>": Provider "<>ToString[provider,InputForm]<>" not found"]];

    If[Names[provider<>"`"<>base<>"`"<>fn] === {},
       Error["Function "<>provider<>"`"<>base<>"`"<>fn<>" not found"]];

    f = ToExpression[provider<>"`"<>base<>"`"<>fn];
    (* Print["calling ", f, " with ", {args}]; *)
    f@@args];

End[];

EndPackage[];
