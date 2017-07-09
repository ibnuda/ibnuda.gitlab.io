module Generate

open System
open System.IO

open Markdig

open Util

let parsePost path =
  let content = read "pages" path
  Markdown.ToHtml content

let generateIndex () =
  let path = "pages"
  ()
