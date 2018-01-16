module Generate

open System.IO

open Markdig

open Conf
open ReadWrite
open View

let parse (info : BlogInfo, content) =
  info, Markdown.ToHtml content

let generateAllPost () =
  Directory.GetFiles (conf.PagesPath)
  |> Seq.map (readMarkdown >> parse >> untuple viewPage)
  |> Seq.sortByDescending (fun (info, _) -> info.Date)
  |> Seq.map writeHtml
  |> viewIndex
  |> htmlToString
  |> writeIndex
