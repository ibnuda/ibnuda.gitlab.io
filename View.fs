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

let maincss =
  link [
    "rel" %= "stylesheet"
    "href" %= "static/main.css"
  ]
let milligramcss =
  link [
    "rel" %= "stylesheet"
    "href" %= "static/milligram.min.css"
  ]
let normalizecss =
  link [
    "rel" %= "stylesheet"
    "href" %= "static/normalize.min.css"
  ]
let somethingJs =
  script [
    "src" %= "static/something.js"
  ]

let sitenav =
  nav [
    "class" %= "navigation"
    section [
      "class" %= "container"
      a [
        "class" %= "navigation-title"
        "href" %= "/"
        text "Home"
      ]
    ]
  ]

let mainView content =
  html [
    head [
      title [ text conf.SiteTitle ]
      metaView
      metaDesc
      metaAuth
      milligramcss
      normalizecss
      maincss
      somethingJs
    ]
    body [
      main [
        "class" %= "wrapper"
        sitenav
        content
      ]
    ]
  ]

let viewIndex titleAndLinks =
  let content =
    section [
      "class" %= "container"
      h3 [ text "Index" ]
      ul [
        for (title, link) in titleAndLinks -> li <| [ a <| href link (text title) ]
      ]
    ]
  mainView content

let viewPage (info : BlogInfo) content =
  let content =
    section [
      "class" %= "container"
      text content
    ]
  info, htmlToString <| mainView content
