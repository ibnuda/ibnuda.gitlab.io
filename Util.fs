module Util

open System
open System.IO

open Conf

let filePath dir path =
  [| Directory.GetCurrentDirectory(); dir; path |] |> Path.Combine

let files dir =
  [| Directory.GetCurrentDirectory (); dir |]
  |> Path.Combine
  |> Directory.GetFiles

let names path =
  Path.GetFileNameWithoutExtension path

let namesHtml path =
  let name = names path
  name + ".html"

let read path =
  use file = File.Open (path, FileMode.Open)
  use reader = new StreamReader (file)
  reader.ReadToEnd ()

let readWithDir dir path =
  let path = filePath dir path
  read path

let delete dir path =
  let path = filePath dir path
  File.Delete path

let deletePreviousPost () =
  conf.PublicPath
  |> Directory.GetFiles
  |> Seq.iter File.Delete

let writeToPublic path content =
  let path = filePath conf.PublicPath path
  File.WriteAllText (path, content)
