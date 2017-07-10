(***
  Copyright: https://github.com/ptrelford/FsHtml
  Modified by me.
  License: Apache License Version 2.0 (http://www.apache.org/licenses/)
 ***)

module FsHtml

open System.Security

type Html =
  | Elem of string * Html list
  | Attr of string * string
  | SingleAttr of string
  | Text of string
  with
  static member ToString elem =
    let rec toString indent elem =
      let spaces = String.replicate indent " "
      match elem with
      | Attr (name,value) -> name + "=\"" + value + "\""
      | SingleAttr name -> name
      | Elem (tag, [ Text s ]) ->
        spaces + "<" + tag + ">" + s + "</" + tag + ">\r\n"
      | Elem (tag, content) ->
        let isAttr =
          function Attr _ | SingleAttr _ -> true | _ -> false
        let attrs, elems = content |> List.partition isAttr
        let attrs =
          if List.isEmpty attrs then ""
          else " " + String.concat " " [ for attr in attrs -> toString 0 attr ]
        match elems with
        | [] -> spaces + "<" + tag + attrs + "/>\r\n"
        | _ ->
          spaces + "<" + tag + attrs + ">\r\n"
          + String.concat "" [ for e in elems -> toString (indent + 1) e ]
          + spaces + "</" + tag + ">\r\n"
      | Text (text) ->
        spaces + text + "\r\n"
    "<!DOCTYPE html>\r\n" + toString 0 elem
  override this.ToString() = Html.ToString this

let elem tag content = Elem(tag,content)
let html = elem "html"
let head = elem "head"
let title = elem "title"
let style = elem "style"
let meta = elem "meta"
let body = elem "body"
let div = elem "div"
let br = elem "br"
let section = elem "section"
let span = elem "span"
let table = elem "table"
let thead = elem "thead"
let tbody = elem "tbody"
let tfoot = elem "tfoot"
let img = elem "img"
let map = elem "map"
let area = elem "area"
let p = elem "p"
let a = elem "a"
let tr = elem "tr"
let td = elem "td"
let th = elem "th"
let ul = elem "ul"
let li = elem "li"
let h1 = elem "h1"
let h2 = elem "h1"
let h3 = elem "h1"
let h4 = elem "h1"
let strong = elem "strong"
let text = Text // SecurityElement.Escape >> Text
let safeText = SecurityElement.Escape
let (%=) name value = Attr (SecurityElement.Escape name, SecurityElement.Escape value)
let single attribute = SingleAttr attribute
let (~%) s = [ text s ]
let areaTeks = [elem "textarea" []]

// an odd ball. But a nice one nonetheless.
let href url content =
  [ "href" %= url
    content ]
