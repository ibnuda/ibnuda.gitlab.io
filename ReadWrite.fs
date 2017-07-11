module ReadWrite

open System
open System.Text.RegularExpressions
open System.IO

open Conf

let untuple f (x, y) =
  f x y

let dateAndTitle info =
  DateTime(info.Date).ToString("yyyyMMdd") + " - " + info.Title

let regexTitle title = Regex.Replace (title, "[^a-zA-Z0-9_.]+","")

let pages = conf.PagesPath
let publi = conf.PublicPath
let pathAndPages path =
  Path.Combine [| pages; path |]
let pathAndPublic path =
  Path.Combine [| publi; path |]

/// Read file on the /pages and returns two twopmost line as BlogInfo
/// and the rest as content of the post.
let readMarkdown path =
  use fs = File.Open (path, FileMode.Open)
  use sr = new StreamReader (fs)
  let title = sr.ReadLine ()
  let date = sr.ReadLine () |> int64
  BlogInfo.Create (title, date), sr.ReadToEnd ()

/// Write html file to /public and then return returns
let writeHtml (info, content) =
  let title = info.Title.ToLower()
  let date = info.Date
  let reptitle = regexTitle title
  let filename = date.ToString () + "_" + reptitle + ".html"
  File.WriteAllText (pathAndPublic filename, content)
  dateAndTitle info, filename

let writeIndex content =
  File.WriteAllText (pathAndPublic "index.html", content)

let writeMarkdown (info : BlogInfo) =
  let filename = (regexTitle info.Title) + ".md"
  printfn "Please open %A" (pathAndPages filename)
  File.WriteAllText (pathAndPages filename, info.ToMd ())

let deleteFiles () =
  Directory.GetFiles (conf.PublicPath)
  |> Seq.iter File.Delete
