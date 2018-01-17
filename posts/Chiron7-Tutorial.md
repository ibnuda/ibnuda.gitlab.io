Chiron 7 Tutorial
2017-07-11 02:00:00 UTC
Chiron 7 Tutorial
=======================

Well, [Chiron](https://github.com/xyncro/chiron) is an F# library for (de)serialization of Json. It's really well written.
If you ever used Elm's json serialization, you will feel at home at no time.

Though I admit, there are a few of ceremonious parts of its usage, especially in enc/dec part.
But that's fine, there are prices that should be paid for flexibility. And furthermore, this library is really fun to be learned from.

## Instalation

In standard netcore project (`.fsproj`), we can easily install it by entering the following command on the command line interface.
```
dotnet add package -v 7.0.0-*
```
And in your `.fsproj` file, you will see something like the following line.
```
<ItemGroup>
  <PackageReference Include="Chiron" Version="7.0.0-*" />
</ItemGroup>
```

## Terminologies:
There are a few terminologies which will be used very often in the rest of the post.

- `Json`: A discriminated union of `JsonObject`, `Json list`, `string`, `number`, `bool`, and `null`.
- `JsonResult<'a>`: Chiron's, using Walschin's term, elevated type. Something like `Option<'a>` but instead of `None` and `Some a` we will have `JsonFailure` and `JPass a`.
- `JsonObject`: I don't know how to word it correctly. But I'm sure it is Chiron's json object type representation.
- `JPass a`: Represent value a in elevated state.
- `JFail`: Instead of `null`, we will have `JsonFailure`, which could be parsing failure. Whether it's type mismatch, deserialization error, invalid json string, or something else.
- `Decoder<'s, 'a>`: Extract `'s` to the `JsonResult<'a>`.
- `ObjectReader<'a>`: A `JsonDecoder<'a>` "creator" with a `JsonObject` as the first type parameter.
- `ObjectBuilder<'a>`: Again, a function to build a `JsonObject` from an `'a` object.
- `JsonDecoder<'a>`: An elevator from type `'a` to `Decoder<'s, 'a>`.
- `JsonEncoder<'a>`: `JsonDecoder<'a>`'s counterpart. From a `Json` elevated state, to normal state.

## Usages
Define an object, as an example we will have a type and a nested type. I took it from my other project ([Forum](https://gitlab.com/ibnuda/Forum/blob/master/src/Libr/Lock/AuthModels.fs)).

### Our Types
```
open Chiron
open Chiron.Operators

module AuthModels =

  type Qualification =
    { Id    : int
      Value : int
      Name  : string }
    static member Create (ide, value, name) =
      { Id    = ide
        Value = value
        Name  = name }

  type Token =
    { Quali     : Qualification
      UserId    : Guid
      ExpiresIn : int64 }
    static member Create (quali, uid, eIn) =
      { Quali     = quali
        UserId    = uid
        ExpiresIn = eIn }

```
In the above code, we have imported `Chiron` and `Chiron.Operators` modules from the Chiron package. We will use that in a few minutes.
And there's an object type, named `Token`, which has `Quali` property, which in turn is a `Qualification`. That type has a static member which will be used in the module which has the same name as the specified object previously.

Then, we write the following code:

```
module Qualification =
  let private create ide value name = Qualification.Create (ide, value, name)
  let encode qualif jsonObj =
    jsonObj
    |> Json.Encode.required Json.Encode.int    "id"    qualif.Id
    |> Json.Encode.required Json.Encode.int    "value" qualif.Value
    |> Json.Encode.required Json.Encode.string "name"  qualif.Name
  let decode =
    create
    <!> Json.Decode.required Json.Decode.int    "id"
    <*> Json.Decode.required Json.Decode.int    "value"
    <*> Json.Decode.required Json.Decode.string "name"

module Token =
  let private create qualif uid ein = Token.Create (qualif, uid, ein)
  let private encodeQualif = Json.Encode.jsonObjectWith Qualification.encode
  let private decodeQualif = Json.Decode.jsonObjectWith Qualification.decode
  let encode token jsonObj =
    jsonObj
    |> Json.Encode.required encodeQualif      "qualification" token.Quali
    |> Json.Encode.required Json.Encode.guid  "userid"        token.UserId
    |> Json.Encode.required Json.Encode.int64 "expiresin"     token.ExpiresIn
  let decode =
    create
    <!> Json.Decode.required decodeQualif      "qualification"
    <*> Json.Decode.required Json.Decode.guid  "userid"
    <*> Json.Decode.required Json.Decode.int64 "expiresin"

```
### Functions' Explanation
There are basically five things there.
```
// create : int -> int -> name -> Qualification
let private create ide value name = Qualification.Create (ide, value, name)
```
This function will be used to create a `Qualification` object in the following code:
```
let decode =
  create
  <!> Json.Decode.required Json.Decode.int    "id"
  <*> Json.Decode.required Json.Decode.int    "value"
  <*> Json.Decode.required Json.Decode.string "name"

```
By defining `decode`, we have created a `Decoder` with two params, `JsonObject` as the input and `Token` as the output.
At the snippet above, there are two operators (which we've imported from `Chiron.Operators`). An operator `<!>` and operator `<*>`.

The `<!>` is a custom operator for `Decoder.map` which has the signature as the following:
```
map : ('a -> 'b) -> Decoder<'s, 'a> -> Decoder<'s, 'b>
```
Just a standard `map` function, which transform a `Decoder` to another `Decoder`.

While `<*>` is a custom operator for `Decoder.apply` which has the signature as the following:
```
apply : Decoder<'s, 'a> -> Decoder<'s, 'a -> 'b> -> Decoder<'s -> 'b>
```
Again, standard `apply` function, which apply a parameter to a function (but in form of a `Decode` object).

So, the `decode` function above, also could be written as:
```
let decode =
  Json.Decoder.map create (Json.Decode.required Json.Decode.int    "id")
  |> Json.Decoder.apply   (Json.Decode.required Json.Decode.int    "value")
  |> Json.Decoder.apply   (Json.Decode.required Json.Decode.string "name")

```
Basically, the `decode` function above means, in particular order of the `create` function's signature, we will take values from a `JsonObject`:

1. Take an `int` value from the `JsonObject` which **should** has a field named `id`.
2. Then take an `int` value from the `JsonObject` which also *should* has a field named `value`.
3. Then take a `string` value from the `JsonObject` which also *should* has a field named `name`.
4. Apply the taken values above, in particular order of the `create` function's signature, to the `create` function.

And the result is a decoder which will transfrom from `JsonObject` to `Qualification`.

Move on to the `encode` part which create an `ObjectBuilder<Qualification>` in the following code.
```
let encode qualif jsonObj =
  jsonObj
  |> Json.Encode.required Json.Encode.int    "id"    qualif.Id
  |> Json.Encode.required Json.Encode.int    "value" qualif.Value
  |> Json.Encode.required Json.Encode.string "name"  qualif.Name

```
The `encode` function above means, we will take a `Qualification` object and another `JsonObject` and then do the following step.

1. Take an `int` value of property `Id` of `qualif` object and put it into `jsonObj` with fieldname `"id"`.
2. Then take an `int` value of property `Value` of `qualif` object and put it into the result of step no 1 with fieldname `"value"`.
3. Then take a `string` value of property `Name` of `qualif` object and put it into the result of step no 2 with fieldname `"name"`.

And the result is an `ObjectBuilder<Qualification>` which can be used to encode a `Qualification` object to a `JsonObject`, which in turn can be parsed to json string in the latter part of the post.


Next part is the explanation of `Token` module's functions. Let's take a look at these two functions.
```
let private encodeQualif = Json.Encode.jsonObjectWith Qualification.encode
let private decodeQualif = Json.Decode.jsonObjectWith Qualification.decode
```
First function, `encodeQualif`, is an encoder from a `Qualification` object to `Json` object. `Json.Encode.jsonObjectWith` itself basically function that takes an `ObjectBuilder` and gives result a `Json` object.
While the second function, `decodeQualif`, is a `Decoder<Json, Qualification>` object, `Json.Decode.jsonObjectWith` is that takes a decoder with input `JsonObject` and modify it to have `Json` input.
Those two previous functions will be used in the following functions.
```
let decode =
  create
  <!> Json.Decode.required decodeQualif      "qualification"
  <*> Json.Decode.required Json.Decode.guid  "userid"
  <*> Json.Decode.required Json.Decode.int64 "expiresin"
  
```
Basically, the `decode` function above means, in particular order of the `create` function's signature, we will take values from a `JsonObject`:

1. Take a `Qualification` value from the `JsonObject` which **should** has a field named `qualification`.
2. Then take a `Guid` value from the `JsonObject` which also *should* has a field named `userid`.
3. Then take a `int64` value from the `JsonObject` which also *should* has a field named `expiresin`.
4. Apply the taken values above, in particular order of the `create` function's signature, to the `create` function.
  
And the result is a decoder which will transfrom from `JsonObject` to `Token`.

```
let encode token jsonObj =
  jsonObj
  |> Json.Encode.required encodeQualif      "qualification" token.Quali
  |> Json.Encode.required Json.Encode.guid  "userid"        token.UserId
  |> Json.Encode.required Json.Encode.int64 "expiresin"     token.ExpiresIn

```
The `encode` function above means, we will take a `Qualification` object and another `JsonObject` and then do the following step.

1. Take a `Qualification` value of property `Qualif` of `token` object and put it into `jsonObj` with fieldname `"qualification"`.
2. Then take a `Guid` value of property `UserId` of `token` object and put it into the result of step no 1 with fieldname `"userid"`.
3. Then take an `int64` value of property `ExpiresIn` of `token` object and put it into the result of step no 2 with fieldname `"expiresin"`.

And the result is an `ObjectBuilder<Token>` which can be used to encode a `Token` object to a `JsonObject`, which in turn can be parsed to json string in the latter part of the post.


## Serialisation and Deserialisation
We will serialize a `Qualification` or a `Token` object to json string, using the following snippet
```
let inline modelToJson (encoder : 'a -> JsonObject -> JsonObject) model =
  model
  |> Json.Encode.jsonObjectWith encoder
  |> Json.format

```
The `modelToJson` function above takes a parameter which has a signature `'a -> JsonObject -> JsonObject` (or an `ObjectBuilder<'a>`) and an instance of object with type `'a`.
That function means:

1. Build a `Json` object with an `ObjectBuilder<'a>` and an instance of `<'a>`.
2. Format it as json string.

```
let inline jsonToModel (decoder : JsonObject -> JsonResult<'a>) jsonstr : 'a option =
  let parsingRes =
    jsonStr
    |> Json.parse
    |> JsonResult.bind (Json.Decode.jsonObjectWith decoder)
  match parsingRes with
  | JPass result -> Some result
  | JFail _      -> None

```
And when we want to deserialise json string to an object, we will use the function `jsonToModel` above which takes a function which tranfrom `JsonObject` to `JsonResult<'a>` (or an `ObjectReader<'a>`) and a string which we assume as a json string. The result of that function is an option type of `'a`.
That function means :

1. Parse the input, `jsonStr`. At this step, the result of the function is `JsonResult<Json>`.
2. If the result parsed is `JPass json`, we will read the `json` using `ObjectReader<'a>`. Else, just return the `JFail jfailure`.
3. Save the result of step 2 to `parsingRes`.
4. If the `parsingRes` succesfully parsed, we will have `Some result`. Else, `None` will be returned.

## Example.
The usage is something like this:
```
let qualif = Qualification.Create (1, 1, "Water") 
let token = Token.Create (qualif, Guid.Empty, 0L)

let jsonFromQualification = modelToJson Qualification.encode qualif

// result (beautified) :
// {
//   "id":1,
//   "value":1,
//   "name":"water"
// }

let jsonFromToken = modelToJson Token.encode token

// result (beautified) : 
//{
//  "qualification": {
//    "id":1,
//    "value":1,
//    "name":"water"
//  },
//  "userid":"00000000-0000-0000-0000-000000000000",
//  "expiresin":0
//}

let stringJsonToken =
"""{"qualification":{"id":1,"value":1,"name":"water"},"userid":"00000000-0000-0000-0000-000000000000","expiresin":0}"""
let stringJsonTokenWithoutUserid =
"""{"qualification":{"id":1,"value":1,"name":"water"},"us":"00000000-0000-0000-0000-000000000000","expiresin":0}"""

let tokenFromJson = jsonToModel Token.decode stringJsonToken
Assert.Equal (Some token, tokenFromJson)
let tokenFailParsed = jsonToModel Token.decode stringJsonTokenWithoutUserid
Assert.Equal (None, tokenFailParsed)

```
