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
  |> Seq.map (readMarkdown >> parse >> untuple viewPage >> writeHtml)
  |> viewIndex
  |> htmlToString
  |> writeIndex
