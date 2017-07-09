open System

open Util
open Generate

[<EntryPoint>]
let main argv =
  parsePost "Something.md"
  |> write "public" "something.html"
  0
