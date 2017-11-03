Authorization in Servant
636452721192300168

Authorization in Servant (WIP)
==============================

Words of caution: This article is about servant-server 0.11 which is still in experimental stage. Deployment in production is not couraged. And yes, I know about `servant-auth`.

One of my friend once complained about the lacks of Servant's documentation on authorization, connecting to db, and many more. So, I want to help him.

## Final Result
- A working REST interface.
- With authentication using JWT.
- And the program can talk with a database.

## Prerequisites.

There are a few things that we will use in this article. Namely:
- A running instance of MariaDB or MySQL. This choice was based on observation that in Indonesia, MySQL is more preferable than Postgres, though I personally choose Postgres any over db.
- `stack`. We will use stack because it prevents cabal hell or something, that's what the internet says about it. So we will heed internet's call this time.

A nice to have setup:
- Emacs with haskell mode. It eases our life a bit.

## Installation.

We will create a servant project using `stack`. A pretty simple command line input will suffice. Something like this:
```
stack new OurServant servant
```
The previous line means that we ask `stack` to create a new project in folder named `OurServant` using `servant` template. There are many other templates, though. You can check it out. It's nice.

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

### Database
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
For each `import` lines above, we tell that we will use their functionality in our sources. And then, we will put the following lines below the previous part. 

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

If we try to compile our project by inputting `stack build` in our root project directory, it will produce error something like `parse error on input '=' perhaps you need a 'let' in a 'do' block?`
It means that GHC doesn't understand that we use `QuasiQuotes` syntax extension in our code. So we will add it at the topmost of our source code.

```
{-# LANGUAGE QuasiQuotes #-}
module Models where

import Data.Aeson
import Data.Text
```

Again, we will receive an error stated that we have a naked expression and perhaps we intended to use TemplateHaskell. So, we'll add that syntax extension!
```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Models where

import Data.Aeson
```
And again, it looks like the GHC refused to compile our source again. GHC suggested that we use `TypeFamilies` extension. So we will give it what it wants!
```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models where
```

But wait, there's more! Because `UsersId` in our data has a specialised result, we have to use `ExistensialQualification` or `GADTs` to allow this. And because we added a `Users json`, which is an instance declaration for `ToJSON`, we have to use `FlexibleInstance`. GHC's suggestion is our command~
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
    by UsersId maxlen=52
    deriving Show Eq
```
It means that we declare that we will create a new table named `super_secrets` which has `something` column and a foreign key `by` which refers to table `users`' primary key.
Alas, when we compile our project, there will be an error stating that it is an illegal instance for `ToBackendKey SqlBackend SuperSecrets` but GHC suggests that we can use `MultiParamTypeClasses` to allow more. So we will reply GHC's call by applying the suggestion.
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
Okay, it's cool and dandy when we compile it. Then, we will create a migration plan (I don't know how does it should be called or its real name).
```
-- src/Models.hs
doMigration = runMigration migrateAll
```
When we compile again, GHC will fail to compile because of ambiguous type variable `m0`. GHC inferred that `doMigration` has `ReaderT SqlBackend m0 ()` as its type and has potential instance of `IO` monad as its fixes. So we'll add that as `doMigration`'s type signature. 

```
-- src/Models.hs
doMigration :: ReaderT SqlBackend IO ()
doMigration = runMigration migrateAll
```

### Auth part.
