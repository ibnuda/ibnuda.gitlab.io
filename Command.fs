module Command

open System

open Conf
open ReadWrite
open Generate

let readline () = Console.ReadLine()

let parseInt64 str =
  match Int64.TryParse str with
  | true, _ -> true
  | _ -> false

let rec askTitle () =
  printf "Title : "
  match readline () with
  | x when String.IsNullOrEmpty x ->
    printfn "Just write something."
    askTitle ()
  | y -> y

let askDate () = // No. I'm taken.
  printf "Date in ticks (press enter for now) : "
  let rec date () =
    match readline () with
    | x when parseInt64 x -> Int64.Parse x
    | x when String.IsNullOrEmpty x -> DateTime.Now.ToUniversalTime().Ticks
    | _ -> date ()
  date ()

// Because I'm bored.
type Com =
  | NewPost
  | Compile
  | CleanPo
  | TheDuck

let read =
  function
  | "newpost" -> NewPost
  | "compile" -> Compile
  | "cleanpo" -> CleanPo
  | _         -> TheDuck

let exec =
  function
  | NewPost ->
    let title = askTitle ()
    let date = askDate ()
    BlogInfo.Create (title, date)
    |> writeMarkdown
  | Compile ->
    generateAllPost ()
    |> printfn "%A"
  | CleanPo ->
    deleteFiles ()
  | TheDuck ->
    printfn "We are not connected by destiny. Bye~"
