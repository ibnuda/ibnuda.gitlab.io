open System

open Conf
open ReadWrite
open Generate

[<EntryPoint>]
let main argv =
  // let (bloginfo, content) = readMarkdown "waterboard.md"
  // let (a, b) = writeHtml bloginfo (parse content)
  // printfn "%s" <| dateAndTitle a
  generateAllPost ()
  |> printfn "%A"
  0
