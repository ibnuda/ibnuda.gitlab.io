module Generate

open System
open System.IO

open Markdig

open Conf
open Util
open View

let parsePost path =
  let content = readWithDir Conf.Default.PagesPath path
  Markdown.ToHtml content

let parse path =
  let content = read path
  Markdown.ToHtml content

let writeHtml path url =
  path
  |> parse
  |> viewPage
  |> htmlToString
  |> writeToPublic url
  url

let generateSite () =
  files Conf.Default.PagesPath
  |> Seq.map (fun x -> (writeHtml x <| namesHtml x), names x)
