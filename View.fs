module View

open FsHtml

let css =
  """
  html { -webkit-text-size-adjust: 100%; padding-bottom: 4em; padding-top: 3em }
  body { font-family: sans-serif; line-height: 1.5em; max-width: 40em; padding: 0 2%; margin: auto }
  fieldset { margin-bottom: 1.5em }
  textarea { height: 10em }
  dt { font-weight: bold }
  .text-input { width: 96%; display: block; padding: .5em 1%; margin-bottom: 1.5em; font-size: 1em }
  .important { color: red }
  .footer { text-align: right }
  .centered { text-align: center }
  """

let siteStyle =
  style [ "type" %= "text/css"
          text css ]
