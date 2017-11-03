Authorization in Servant
636452721192300168

Authorization in Servant: Walkthrough (WIP)
===========================================

Words of caution: This article is about servant-server 0.11's `experimental-auth` which is still in experimental stage.
Deployment in production is not couraged.
And yes, I know about `servant-auth` but I haven't read it thoroughfully, yet.

One of my friend once complained about the lacks of Servant's documentation on authorization, connecting to db, and many more.
So, I want to help him.

## Minimum Requirements
- Understands basic haskell.

## Final Result
- A working REST interface.
  ```
  /auth          POST -> requesting for jwt token.
  /secrets       POST -> creating new secret. (JWT Auth.)
  /secrets/:user GET  -> get secrets by username. (JWT Auth.)
  ```
- With authentication using JWT.
- And the program can talk with a database.
- You can look at this [repo](https://gitlab.com/ibnuda/Servant-Auth-Walkthrough) for the final result (still in WIP tho.)

## Prerequisites.

There are a few things that we will use in this article.
Namely:
- A running instance of MariaDB or MySQL.
This choice was based on observation that in Indonesia, MySQL is more preferable than Postgres, though I personally choose Postgres any over db.
- `stack`.
We will use stack because it prevents cabal hell or something, that's what the internet says about it.
So we will heed internet's call this time.

A nice to have setup:
- Emacs with haskell mode. It eases our life a bit.

## Installation.

We will create a servant project using `stack`.
A pretty simple command line input will suffice.
Something like this:
```
stack new OurServant servant
```
The previous line means that we ask `stack` to create a new project in folder named `OurServant` using `servant` template.
There are many other templates, though.
You can check it out.
It's nice.

Then we will change our directory to our project's directory and open an emacs instance there.
```
cd OurServant; emacs . -nw
```

Navigate to `OurServant.cabal` and then you will see a part like the following:
```
library
  hs-source-dirs:     src
  exposed-modules:    Lib
  build-depends:      base >= 4.7 && < 5
                    , aeson
                    , servant-server
                    , wai
                    , warp
  default-language:   Haskell2010
```
Then you add dependencies: 
- `text` is, in short, an library for efficient unicode text
- `time` well, it provides time!
- `peristent` is... I guess something like `django-orm` or Scala's `slick` library.
- `persistent-mysql` provides a MySQL backend for `persistent`.
- `persistent-template` is a  provider of Template Haskell for `persistent`, I will not touch what does it mean in this article.

And let's add a few other lines in our cabal file so it will look like the following:
```
library
  hs-source-dirs:     src
  exposed-modules:    Lib
  other-modules:      Models -- new
  build-depends:      base >= 4.7 && < 5
                    , aeson
                    , persistent --new
                    , persistent-mysql --new
                    , persistent-template --new
                    , servant-server
                    , text --new
                    , wai
                    , warp
  default-language:   Haskell2010
```

## Writing Code

### Database and Models
Then we will create a new haskell source file named `Models.hs` in `src` directory.

```
-- src/Models.hs
module Models where

import Data.Aeson -- json (de)serialisation.
import Data.Text
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH

```
For each `import` lines above, we tell that we will use their functionality in our sources.
And then, we will put the following lines below the previous part. 

```
-- src/Models.hs
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Users json
    name Text sqltype=varchar(52)
    pass Text sqltype=varchar(52)
    Primary name
    -- Unique of table Users named Name referring column name.
    UniqueUsersName name
    deriving show
|]
```
The part of source code above means that we will create a migration plan named `migrateAll` by creating tables `users` which has `name` and `pass` columns and column `name` will be unique and will be used as the primary key.

If we try to compile our project by inputting `stack build` at our root project directory, it will produce error something like `parse error on input '=' perhaps you need a 'let' in a 'do' block?`
It means that GHC doesn't understand that we use `QuasiQuotes` syntax extension in our code.
So we will add it at the topmost of our source code.

```
{-# LANGUAGE QuasiQuotes #-}
module Models where

import Data.Aeson
import Data.Text
```

Again, we will receive an error stated that we have a naked expression and perhaps we intended to use TemplateHaskell.
So, we'll add that syntax extension!
```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Models where

import Data.Aeson
```
And again, it looks like the GHC refused to compile our source again.
GHC suggested that we use `TypeFamilies` extension.
So we will give it what it wants!
```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where
```

But wait, there's more! Because `UsersId` in our data has a specialised result, we have to use `ExistensialQualification` or `GADTs` to allow this.
And because we have added a `Users json`, which is an instance declaration for `ToJSON`, we have to use `FlexibleInstance`.
GHC's suggestions are our commands~
```
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where
```
We can compile it just fine! And then, we will create a new table.
```
-- src/Models.hs
    UniqueUsersName name
    deriving Show Eq
  SuperSecrets json
    something Text
    at UTCTime
    by UsersId maxlen=52
    deriving Show Eq
```
It means that we declare that we will create a new table named `super_secrets` which has `something` column, a column with `datetime` type, and a foreign key `by` which refers to table `users`' primary key.
Alas, when we compile our project, there will be an error stating that it is an illegal instance for `ToBackendKey SqlBackend SuperSecrets` but GHC suggests that we can use `MultiParamTypeClasses` to allow more.
So we will reply GHC's call by applying the suggestion.
```
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where
```
Okay, it's cool and dandy when we compile it.
Then, we will create a migration plan (I don't know how should it be called or its real name).
```
-- src/Models.hs
doMigration = runMigration migrateAll
```
When we compile again, GHC will fail to compile because of ambiguous type variable `m0`.
GHC inferred that `doMigration` has `ReaderT SqlBackend m0 ()` as its type and has potential instance of `IO` monad as its fixes.
So we'll add that as `doMigration`'s type signature. 

```
-- src/Models.hs
doMigration :: ReaderT SqlBackend IO ()
doMigration = runMigration migrateAll
```

The next step is we will create a model for our reply token and `POST` data for our REST interface.
In order to be able to encode our data to Json, `AuthUser` have to derive `Generic`.
Which in turn, we have to import `GHC.Generics`.
And then GHC will suggest that we have to use `DeriveGeneric` extension.
So we will do that!
```
-- src/Models.hs
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
-- snip!
data AuthUser = AuthUser
  { authName :: Text
  , token    :: Maybe Text
  } deriving (Show, Generic, Eq)
data UsersSecret = UsersSecret
  { something :: Text
  } deriving (Show, Generic)

instance FromJSON AuthUser
instance ToJSON AuthUser
instance FromJSON UsersSecret
instance ToJSON UsersSecret
```

We don't have to create instances of `FromJSON` and `ToJSON` for our `Users` and `SuperSecrets` because in our template above, we have already declared that!

### Communicating to Database.
We will take a shortcut without reading outside config whatsoever.
That means, we will hardcode our database connection string etc to our code.

Next, we create a file named `DB.hs` in our `src` directory and then open it in our editor.

According to `persistent-mysql` [documentation](https://hackage.haskell.org/package/persistent-mysql-1.2.1/docs/Database-Persist-MySQL.html#t:ConnectInfo), we can create a connection pool to mysql using `createMySQLPool` which takes a `ConnectInfo` and an integer that represents number of pool connections.
```
-- src/DB.hs
module DB where

import Database.Persist.MySQL

createPool = createMySQLPool connection 5
  where
    connection =
      defaultConnectInfo
      { connectHost = "localhost"
      , connectPort = fromIntegral 3306
      , connectUser = "ibnu"
      , connectPassword = "jaran"
      , connectDatabase = "owo"
      }
```
When we compile that, we will receive an error that states that we have an error of ambiguous type.
We can easily supress that error by defining our `createPool`'s signature.
But when we give our function a signature (`IO ConnectionPool`), we will receive an error that no instance of `MonadLogger IO` in our function.
Again, that is an easy problem.
We can import `Control.Monad.Logger` and put `runStdoutLoggingT` (a stdout logger transformer) in front of `createMySQLPool`.
So, the final shape of the function is like the following snippet.
```
-- src/DB.hs
import Control.Monad.Logger

createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $ createMySQLPool connection 5
```

After we have a wrapper for our connection pool, then we will create a query runner.
That is, a function that takes a query and then execute it.
```
-- src/DB.hs
runQuery query = do
  pool <- createPool
  runSqlPool query pool
```
In order to be able to query, we have to import `Database.Persist`.
And we will also create a normal sql query for looking a user in our db by username and password.
```
-- src/DB.hs
import Data.Text -- for our functions' signatures.
import Database.Persist
-- snip
lookUserByUsernameAndPassword :: Text -> Text -> IO (Maybe Users)
lookUserByUsernameAndPassword username password = do
  mUser <- runQuery $ selectFirst [UsersName ==. username, UsersPass ==. password] []
  case mUser of 
    Nothing -> return Nothing
    Just user -> return $ Just $ entityVal user
```
A little explanation:
- `lookUserByUsernameAndPassword` is a function that takes two `Text` parameters which return an IO wrapper of a thing that is an instance of `Users` if there's a row in db that matches the parameters.
Or nothing if there is no matches.
- `mUser` is a result of wrapped computation of the database querying.
- `runQuery`: our query runner, which takes the next query.
- `selectFirst [UsersName ==. username, UsersPass ==. password][]` is our query.
  - `selectFirst` means we only take at most 1 result.
  - Symbol `==.` denotes equality in our query.
  - `UsersName` and `UsersPass` denotes the parts in our "template" above. `Users` part refers to table `users` and `Name` and `Pass` refers to column `name` and `pass`. 
  - Empty square brackets can be used as ordering the data or limit or your normal query options.
- Because there's a probability that there's no information in our table that satisfies our requirement, we can query have to check our result.
- If there result is `Nothing` or there's no user like that, we will return `Nothing`.
- Else, we will return the entity value of our result query.

Then we will create an insert and a get function to `super_secrets` table.
```
-- src/DB.hs
import Data.Text hiding (map)
import Data.Time
-- snip
lookSecretByUsername :: Text -> IO [SuperSecrets]
lookSecretByUsername username = do
  somes <- runQuery $ selectList [SuperSecretsBy ==. (UsersKey username)]
  return $ map entityVal somes
insertSecret :: Text -> UsersSecret -> IO (Key SuperSecrets)
insertSecret username usersSecret = do
  now <- getCurrentTime
  runQuery $
    insert $ SuperSecrets (something UsersSecret) now (UsersKey username)
```
A little explanation for first function:
- We hide `map` from text because it makes function `map` ambiguous (the other is from `Prelude`).
- We import `Data.Time` for getting current time.
- `lookSecretByUsername :: Text -> IO [SuperSecrets]` is the signature of that function. It takes `Text` as a parameter and returns an `SuperSecrets` list wrapped in `IO` wrapper.
- `somes` is the result of the wrapped computation of query execution by `runQuery`
- `selectList [SuperSecretsBy ==. (UsersKey username)] []`:
  - `selectList` gets all records in DB which satisfy the query.
  - `SuperSecretsBy` represents column `by` in table `super_secrets` which is a foreing key to `users`.`name`.
  - Symbol `==.` denotes equality.
  - `(UsersKey username)` means a primary key with value `username`.
- And then we return a `map`ed of `entitiyVal`ues of the computation result.
Explanation for the second function:
- `insertSecret :: Text -> UsersSecret -> IO (Key SuperSecrets)` is the signature of the function. Which is a function that takes a `Text` and a `UsersSecret` as parameters then return a wrapped primary key of the inserted row.
- `now` is the result of computation of `getCurrentTime`. `now` itself is an `UTCTime`.
- then we `insert` `something` from field `UsersSecret` into column `something`, `now` into `at` column, and `UserKey username` into foreign key `by` of `SuperSecrets` table.

> What we've done so far, has been committed to git. Check it [here](https://gitlab.com/ibnuda/Servant-Auth-Walkthrough/tree/2ecf7bb1438c5d1c2c7ef32745f639f96d3d3634).

### Auth (JWT)

Our auth process is:
- A http request comes from.
- Server parse its payload.
- If its payload is valid, then the server authenticate the payload with the existing data (could be from DB or whatever) and then return an auth json object with token.
- If the payload doesn't match, then return an empty auth json object without token.

We will use JWT for our authentication and/or authorization framework. So, we will create a new source file named `Auth.hs`.
But firstly, we have to define what kind of payload we will send and receive. So, let's say something like this:
```
{
  "exp": int64, --seconds since unix epoch.
  "iat": int64, --seconds since unix epoch.
  "jti": guid,
  "iss": string,
  "sub": string
  "name": string, -- an unregistered claim.
}
```
Because we have decided that we will use unix' epoch and guid, we will add `guid` and `jwt` packages into our dependencies. Don't forget to add `Auth` into `other-modules`.

```
-- cabal file.
  other-modules:      Models
                    , DB
                    , Auth --new
  build-depends:      base >= 4.7 && < 5
                    , aeson
                    , jwt --new
                    , guid --new
```
Because stack is unable to resolve `guid`, we have to input `stack solver --update-config` at our shell in our root directory and then we input `stack build` in shell.

So, we will edit `src/Auth.hs` in our editor.
```
-- src/Auth.hs
module Auth where
import Data.Time
import Data.Time.Clock.POSIX -- for our jwt's exp and iat.

nowPosix :: IO POSIXTime
nowPosix = do
  now <- getCurrentTime
  return $ utcTimeToPOSIXSeconds now
```
the explanation's standard, `nowPosix` is a wrapper for the amount of seconds that have passed since unix' epoch.

And then we will write our token creation function.

```
--src/Auth.hs
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.GUID
import Data.Map as Map -- insert package `containers` into your dependecies in your cabal file.
import Prelude hiding (exp)
import Web.JWT
-- snip
createToken :: Users -> IO AuthUser
createToken user = do
  now <- nowPosix -- the previous function.
  guid <- genText -- from Data.GUID
  let creation = numericDate $ now
      expiration = numericDate $ now + 60
      claims =
        def
        { exp = expiration
        , iat = creation
        , iss = stringOrURI "issuer"
        , jti = stringOrURI guid
        , sub = stringOrURI "localhost"
        , unregisteredClaims =
            Map.fromList [ ("name", String $ usersName user)]
        }
      key = secret "Indonesia Raya"
      token = encodeSigned HS256 key claims
  return $ AuthUser (usersName user) token
```
Explanation
- We use `OverloadedStrings` extension to tell GHC to regards `[Char]` or `String` as `Text`.
- We import `Data.GUID` and `Web.JWT` while hiding function `exp` from `Prelude`. 
We do that because the two former is required by our function.
And hiding `exp` because conflicting function from `JWT`.
- `now` and `guid` are the results of the respective computational wrapper.
- `creation` and `expiration` are for our `iat` and `exp` JWT payload's claims.
- `stringOrURI` is a function from `Web.JWT` which takes a `Text` and returns `JWTClaimsSet`.
- `claims` is an instance of `JWTClaimsSet` from `Web.JWT` package.
- `unregisteredClaims` is used for our `claims`.
It is has signature as `[(Text, Value)]`, while `Value` itself is a representation of Haskell value as JSON object by `aeson` library.
- `key` is our secret keys for jwt encription.
- `token` is the result of signed JWT encoding by HS256 encription.
- this function returns an `AuthUser` object.

Then we will use the function above to match the query result from DB.
```
-- src/Auth.hs
-- snip
createTokenForUser :: Maybe Users -> IO AuthUser
createTokenForUser Nothing = return $ AuthUser "" ""
createTokenForUser (Just user) = createToken user
```

> What we've done so far, has been committed to git. Check it [here](https://gitlab.com/ibnuda/Servant-Auth-Walkthrough/tree/a9e60e057b1dc00fc3a4793d0058d1525710a8fb).

### REST interface using Servant.

So, here we are, we will design our REST interface. So, navigate to `src/Lib.hs` and delete the content.
```language=haskell
-- src/Lib.hs
module Lib where

import Data.Text -- To be able to use Text
import Servant  -- Servant's functions. Like, :>, :<|>, etc.
import Servant.Server.Experimental.Auth -- Auth

import Auth
import Models
import DB

type instance AuthServerData (AuthProtect "jwt-auth") = Users

type TopSekrit =
       "auth"
       :> ReqBody '[ JSON] Users
       :> Post '[ JSON] AuthUser
  :<|> "secrets"
       :> AuthProtect "jwt-auth"
       :> ReqBody '[ JSON] UsersSecret
       :> Post '[ JSON] ()
  :<|> "secrets"
       :> Capture "username" Text
       :> AuthProtect "jwt-auth"
       :> ReqBody '[ JSON] UsersSecret
       :> Get '[ JSON] [SuperSecrets]
```
If you compile the snippet above, you will get a lot of errors.
For example, GHC suggests that we have to use `DataKinds`.
And when have added that, we will get another error about `illegal operators` and how to fix it by adding `TypeOperators` extension.
Which in turn, another error appeared, `illegal family instance` and how to fix it by adding `TypeFamilies` extension.
After we've added those three extensions at the topmost source file, the source will be looked like this, and compiled just fine.

```language=haskell
-- src/Lib.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Lib where
```
And the explanation of the snippet above is:
- `type instance AuthServerData (AuthProtect "jwt-auth") = Users`
  Sorry, without restorting to Stack Overflow [answer](https://stackoverflow.com/questions/20870432/type-family-vs-data-family-in-brief#comment31322081_20871880)
  and `AuthServerData` [documentation](https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Experimental-Auth.html#t:AuthServerData)
  I can't explain it easily.
  Basically, `type instance F A = B` means that an argument `A` that being applied to function `F` equals to `B`.
  So, in this case, the value of the returned data of `(AuthProtect "jwt)` must be equal to `Users`.
- `type TopSekrit = ...` is the type of our REST interface. Which in turn is a result of the composition of the following:
  - `"auth" :> ReqBody '[JSON] Users :> Post '[JSON] AuthUser` means that a POST request json payload value of `Users` at `/auth` will have response the json value of `AuthUser`.
  - `:<|>` is a composition of two API.
  - `"secrets" :> AuthProtect "jwt-auth" :> ReqBody '[JSON] UsersSecret :> Post '[JSON] ()` means that a POST request json payload value of `UsersSecret` at `/secrets` will be received and processed by server as long as the request has been authorized.
  - `"secrets" :> Capture "username" Text :> AuthProtect "jwt-auth" :> Get '[JSON] [SuperSecrets]` means that a GET request at `/secret/:username` will be responded by json values of `[SuperSecrets]` as long as the request has been authorized.

And then, we will create `Context` for our auth protected resources, where `Context` itself is *TO BE ADDED, in short a holder*

So, we will create our `authContextCreator`:

```
- src/Lib.hs
{-# Language FlexibleContexts #-}
import Control.Error.Class
import Network.Wai

authContext :: Context '[AuthHandler Request Users]
authContext = mkAuthHandler authHandler :. EmptyContext
  where
    authHandler :: (MonadError ServantErr m) => Request -> m Users
    authHandler req =
      case lookup "Authorization" (requestHeaders req) of
        Nothing -> throwError err401
        Just token -> undefined -- reserved for token validation.
```
Explanation:
- We use `FlexibleContexts` extension because `throwError` has signature of `ServantErr m a` while our the result of token validation should be `Handler Users`.
  So, to simplify a bit, we force `throwError` to have signature `ServantErr m Users` by using `FlexibleContexts` extension.
- we import `Control.Error.Class`, not really important, but it will give us a clearer signature for our functions.
- we import `Network.Wai` to intercept incoming requests to check its headers. Which will be explained in the next section.
- `authContext :: Context '[AuthHandler Request Users]` is the signature for the next point.
  It means that we will have a context which only contains an `AuthHandler` that accepts a `Request` and returns a `Users`.
- `authContext = mkAuthhandler authHandler :. EmptyContext` we will create a custom handler, which is a request interceptor and should return `Users` (as defined in function signature), and then add it to an `EmptyContext`.
- `authHandler :: (MonadError ServantErr m) => Request -> m Users`, as defined at `authContext`'s signature, this function has to return a wrapped `Users` object after it receives a `Request` and wrapper `m` has to be an instance of `ServantErr`.
- `authHandler req = case lookup "Authorization" (requestHeaders req) of` means that when it receives a `Request`, it will look at the `Request`'s headers. `Authorization` header, to be exact.
- If there's no `Authorization` header, it will throw an 401 error.
- When there's `Authorization` header, it will process the `Request`'s header value to.... `undefined` at the moment.

> What we've done so far, has been committed to git. Check it [here](https://gitlab.com/ibnuda/Servant-Auth-Walkthrough/tree/5001961eda92637a845357d79ec14a6a6ce69e2b).

So, let's open `src/Auth.hs`
