Chiron 7 Tutorial
636353391743216803

Chiron 7 Tutorial
=======================

Well, [Chiron](https://github.com/xyncro/chiron) is an F# library for (de)serialization of Json. It's really well written.
If you ever used Elm's json serialization, you will feel at home at no time.

Though I admit, there are a few ceremonious parts of its usage, especially in enc/dec part.
But that's fine, there are prices that should be paid for flexibility.

## Example (WIP)
Define an object, as an example (I took it from my other project [Forum.Auth](https://gitlab.com/ibnuda/Forum/blob/master/src/Auth/AuthModels.fs)):

```
open Chiron
open Chiron.Operators

module AuthModels =

  type Token =
    { Quali : string
      UserId : Guid
      ExpiresIn : int64 }
    static member Create (quali, uid, eIn) =
      { Quali = quali
        UserId = uid
        ExpiresIn = eIn }

  module TokenEncDec =
    let private create qualif uid ein = Token.Create (qualif, uid, ein)
    let encode token jsonObj =
      jsonObj
      |> Json.Encode.required Json.Encode.string "qualification" token.Quali
      |> Json.Encode.required Json.Encode.guid "userid" token.UserId
      |> Json.Encode.required Json.Encode.int64 "expiresin" token.ExpiresIn
    let decode =
      create
      <!> Json.Decode.required Json.Decode.string "qualification"
      <*> Json.Decode.required Json.Decode.guid "userid"
      <*> Json.Decode.required Json.Decode.int64 "expiresin"

```
And to serialize a `Token` object to a json string:

```
  let inline jsonToModel (decoder : JsonObject -> JsonResult<'T>) jsonstr : 'T option =
    let parsingRes =
      jsonStr
      |> Json.parse
      |> JsonResult.bind (Json.Decode.jsonObjectWith decoder)
    match parsingRes with
    | JPass result -> Some result
    | JFail _      -> None

  let inline modelToJson (encoder : 'T -> JsonObject -> JsonObject) model =
    model
    |> Json.Encode.jsonObjectWith encoder
    |> Json.format

  let tokenToJson = modelToJson TokenEncDec.encode
  let jsonToToken = jsonToModel TokenEncDec.decode
```

The usage is something like this:
```
  let tokenA = Token.Create ("qualif", Guid.Empty, 0L)

  let jsonOfToken = tokenToJson tokenA
  // should result {"qualification":"qualif","userid":"00000000-0000-0000-0000-000000000000","expiresin":0}

  let tokenFromJsonOfToken = jsonToToken jsonOfToken
  // should have result `tokenFromJsonOfToken = Some tokenA`

  // and the following json
  // {"qualification":"qualif","userid":"00000000-0000-0000-0000-000000000000","expiresin":0
  // should have result None
```

## TODO
- Step by step explanation, e.g. why use `Json.Decode.required` and `Json.Decode.string`.
