open System

open Conf
open ReadWrite
open Command

[<EntryPoint>]
let main argv =
  printfn "Pick one of the following commands:"
  printfn "newpost"
  printfn "compile"
  readline () |> read |> exec
  0
