
module SYM = Symbol
module TK  = Token
module HSY = HsSyntax

let errors : string Queue.t = Queue.create ()

let fail msg =
  Queue.add msg errors;
  (fun _ -> None)

let theModidStack : SYM.t Stack.t = Stack.create ()

let begin_parse_module modid =
  Stack.push modid theModidStack;
  theModidStack

let current_modid () =
  Stack.top theModidStack

let q_not_qual = TK.with_region (fun s -> HSY.unqual_id (current_modid (), s))

