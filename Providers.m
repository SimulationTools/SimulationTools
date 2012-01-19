
BeginPackage["Providers`"];

CallProvidedFunction;

Begin["`Private`"];

CallProvidedFunction[base_String, fn_String, args_List, method_:Automatic] :=
  Module[
    {providerHaveFns, providers, provider, have, posns, f},

    providerHaveFns = Names["*`"<>base<>"`HaveData"];
    providers = Map[First[StringSplit[#,"`"]] &, providerHaveFns];

    provider =
    If[method === Automatic,
       (* providers = ToExpression/@providerFns; *)
       have = Map[ToExpression[#]@@args &, providerHaveFns];
       posns = Position[have, True];
       If[Length[posns] > 1,
          Throw["Multiple data sources for "<>base<>" ("<>
                ToString[providers,InputForm]<>
                "): Please select one using Method -> source, where source is one of " <>
                ToString[providers]]];

       If[Length[posns] == 0,
          Throw["No data for "<>base<>"`"<>fn<>"["<>
                StringJoin[Riffle[ToString[#,InputForm]&/@args,","]]<>"]"]];
       providers[[posns[[1,1]]]],
       method];

    If[!MemberQ[providers, provider],
       Throw[base<>": Provider "<>ToString[provider,InputForm]<>" not found"]];

    f = ToExpression[provider<>"`"<>base<>"`"<>fn];
    f@@args];

End[];

EndPackage[];
