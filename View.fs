module View

open Conf
open FsHtml

let htmlToString = FsHtml.Html.ToString

let meta name value =
  meta [
    "name" %= (safeText name)
    "content" %= (safeText value)
  ]

let metaView = meta "viewport" "width=device-width, initial-scale=1.0"
let metaDesc = meta "description" "page content."
let metaAuth = meta "author" conf.Author

let sitecss =
  link [
    "rel" %= "stylesheet"
    "href" %= "static/site.css"
  ]

let siteheader =
  div [
    "class" %= "header"
    a <| href conf.Site (text "Home")
  ]

let mainView content =
  html [
    head [
      title [ text conf.SiteTitle ]
      metaView
      metaDesc
      metaAuth
      sitecss
    ]
    body [
      div [
        "class" %= "page"
        siteheader
        content
      ]
    ]
  ]

let viewIndex titleAndLinks =
  let content =
    div [
      "class" %= "body"
      h3 [ text "Index" ]
      ul [
        for (title, link) in titleAndLinks -> li <| [ a <| href link (text title) ]
      ]
    ]
  mainView content

let viewPage (info : BlogInfo) content =
  let content =
    div [
      "class" %= "body"
      text content
    ]
  info, htmlToString <| mainView content

