module Util

open System
open System.IO

let filePath dir path =
  [| Directory.GetCurrentDirectory(); dir; path |] |> Path.Combine

let read dir path =
  let path = filePath dir path
  use file = File.Open (path, FileMode.Open)
  use reader = new StreamReader(file)
  reader.ReadToEnd ()

let delete dir path =
  let path = filePath dir path
  File.Delete path

let write dir path content =
  let path = filePath dir path
  File.WriteAllText (path, content)
