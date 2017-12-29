open Command

[<EntryPoint>]
let main _ =
  printfn "Pick one of the following commands:"
  printfn "For new post : newpost"
  printfn "For compile  : compile"
  printfn "For cleaning : cleanpo"
  readline () |> read |> exec
  0
