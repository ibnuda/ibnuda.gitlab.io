module View

open Conf
open FsHtml

let css =
  """html { -webkit-text-size-adjust: 100%; padding-bottom: 4em; padding-top: 3em }
  body { font-family: sans-serif; line-height: 1.5em; max-width: 40em; padding: 0 2%; margin: auto }
  fieldset { margin-bottom: 1.5em }
  textarea { height: 10em }
  dt { font-weight: bold }
  .text-input { width: 96%; display: block; padding: .5em 1%; margin-bottom: 1.5em; font-size: 1em }
  .important { color: red }
  .footer { text-align: right }
  .centered { text-align: center }"""

let meta name value =
  meta [
    "name" %= (safeText name)
    "content" %= (safeText value)
  ]

let metaView = meta "viewport" "width=device-width, initial-scale=1.0"
let metaDesc = meta "description" "page content."
let metaAuth = meta "author" Conf.Default.Author

let sitecss =
  style [
    "type" %= "text/css"
    text css
  ]

let viewIndex titleAndLinks =
  html [
    head [
      title [ text Conf.Default.Site ]
      metaView
      metaDesc
      metaAuth
      sitecss
    ]
    body [
      h1 [ text "Index" ]
      ul [
        for (link, title) in titleAndLinks -> li <| [ a <| href link (text title) ]
      ]
    ]
  ]

let viewPage content =
  html [
    head [
      title [ text "Title" ]
      metaView
      metaDesc
      metaAuth
      sitecss
    ]
    body [
      text content
    ]
  ]

let htmlToString = FsHtml.Html.ToString
