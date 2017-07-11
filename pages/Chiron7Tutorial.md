Chiron 7 Tutorial
636353391743216803

Chiron 7 Tutorial (WIP)
=======================

Well, Chiron is an F# library for (de)serialization of Json. It's really well written.
If you ever use Elm's json serialization, you will feel at home at no time.

Though I admit, there are a few ceremonious parts of its usage, especially in enc/dec part.
But that's fine, there are prices that should be paid for flexibility.

## Example (WIP)

Define an object, as an example (I took it from my other project [Forum.Auth](https://gitlab.com/ibnuda/Forum/blob/master/src/Auth/AuthModels.fs)):

```
open Chiron
open Chiron.Operators

module AuthModels =

  module D = Json.Decode
  module E = Json.Encode
  module ID = Inference.Json.Decode
  module IE = Inference.Json.Encode

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
      |> E.required E.string "qualification" token.Quali
      |> E.required E.guid "userid" token.UserId
      |> E.required E.int64 "expiresin" token.ExpiresIn
    let decode =
      create
      <!> D.required D.string "qualification"
      <*> D.required D.guid "userid"
      <*> D.required D.int64 "expiresin"

```
And to serialize a `Token` object to a json string:

```
  let inline modelToJson (encoder : 'T -> JsonObject -> JsonObject) model =
    model
    |> Json.Encode.jsonObjectWith encoder
    |> Json.format

  let tokenToJson = modelToJson TokenEncDec.encode
```
