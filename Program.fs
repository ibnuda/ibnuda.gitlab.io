open System

open Conf
open Util
open FsHtml
open View
open Generate

[<EntryPoint>]
let main argv =
  generateSite ()
  |> viewIndex
  |> htmlToString
  // |> printfn "%A"
  |> writeToPublic "index.html"
  0
