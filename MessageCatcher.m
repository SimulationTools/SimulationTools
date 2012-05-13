
BeginPackage["MessageCatcher`", {"Error`"}];

WithCaughtMessages::usage = "WithCaughtMessages[expr] evaluates expr, converting any generated messages into exceptions";

Begin["`Private`"];

$allowedMessages = {};

(****************************************************************)
(* messageHandler *)
(****************************************************************)

messageHandler[x_] :=
  If[Last[x] && !MemberQ[$allowedMessages,x[[1,1]]],
     x /. HoldPattern[_[Message[id_, args___], _]] :> ErrorMessage[id, args]; Abort[]];

(****************************************************************)
(* WithCaughtMessages *)
(****************************************************************)

SetAttributes[WithCaughtMessages, HoldFirst];
WithCaughtMessages[expr_] :=
  Internal`HandlerBlock[{"Message", messageHandler},
                        (expr)];

End[];

EndPackage[];
