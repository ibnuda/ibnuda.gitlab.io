Cloning FluxBB
2018-04-14 16:38:19.675105163 UTC
# Cloning FluxBB (In Progress)
 
Writing a forum software is one of a few things that I-wished-I-have-but-haven't
in the last of couple of years.
For example, among a gazillion abandoned repos on this GitLab account,
[`Forum`](https://gitlab.com/ibnuda/Forum) is one of them.
If you take a glance on it, you will see -- obviously -- that it's an unfinished
forum software.

So, I decided to take a different approach to finally do it, no matter what it takes.
I will use the following tech:

- FluxBB's database structure. Though in this case, it will be mutilated gruesomely.
- Yesod. A great framework with bells and whistles.
- Esqueleto. See, it's nice to be able to join tables in a bar and/or database.

Okay, before we start to write this clone, we should ensure a few things.
For example, the structure of the tables of this program and what one usually
expects when they use and/or administer a forum software.

### Database Design

Because forum software basically is just a CRUD program, we have to define it first.
Fortunately, [FluxBB's database structure](https://fluxbb.org/docs/v1.5/dbstructure)
is not really complicated.
Not only that, a lot of its fields are `nullable`, so we can be remove them in this
clone.

For example, we will not use some of nullables of `Users`'s columns.
So, instead of [this](https://fluxbb.org/docs/v1.5/dbstructure#users), we will
use (in `persistent`'s fields)
```
Users
  groupId GroupsId
  username Text
  email Text
  password Maybe Text -- we will discuss it later.
  joinTime UTCTime
  topicStarted Int default=0
  repliesPosted Int default=0
  UniqueUsername username -- we will also discuss it later.
  UniqueEmail email
  deriving Show Eq

```

Furthermore, we will not use a few FluxBB's tables. For example, we won't use
`permissions`, `forum_subscribtions`, and `topic_subscribtions`.

### Inner Workings

In real FluxBB's use case, an administator could set a set of permissions for
a certain groups (`moderator`, for example) to do something peculiar like capable
of banning users, but not editing users' profiles.
We will not give the users and administator these kind of features it's too
complicated for my purpose.

So, instead of the actual FluxBB's features, we will dumb it down like the following:

- <b>Users can</b>

  - Read `topic`s and `forum`s.
  - Create `topic` (or `thread` or `discussion`).
  - Reply `topic`.
  - Edit his own `reply`
  - Edit information.
  - Can `report` people.

- <b>Moderators</b>

  - User's capabilities plus
  - Can lock thread.
  - Can ban users.
  - Can unban users.
  - Can respond to `reports`.

- <b>Administrators</b>

  - Moderator's capabilities plus
  - Can create `forum`.
  - Can create `category`.
  - Can promote user to become a Moderator or Administrator.
  - Can demote user.

- <b>Banneds</b>

  - Only logout.
  
### Layout and UI

I'll make a confession, I don't understand CSS and JavaScript.
So, I will use a small CSS framework, [milligram](http://milligram.io/) as helper
and no JS in this program.
That means, we will treat this forum's interface (HTML) as purely a document presentation.
Baring a few buttons and text inputs.
I really hope you don't mind about it.

### The Approach

We will build up the forum from `yesod-minimal` template from stack and then slowly
turn it into `yesod-postgres` and then to a FluxBB's clone.
While we're at it, I will try my best to explain why we do what we do.

## Start Typing

### Project Setup

First of all, I will assume that you will use stack as your build tool.
Okay, let's start typing.

We will create a `yesod-minimal` project by typing `stack new [our-project] yesod-minimal`
in our terminal emulator.
After that, we will see a few files along these lines:
```
.
├── app
│   └── Main.hs
├── Cirkeltrek.cabal
├── package.yaml
├── README.md
├── routes
├── src
│   ├── Add.hs
│   ├── Application.hs
│   ├── Foundation.hs
│   └── Home.hs
└── stack.yaml

```
First, we will edit `stack.yaml` file to add an external dependency.
We will add the following lines
```
extra-deps:
- git: https://github.com/bitemyapp/esqueleto
  commit: b81e0d951e510ebffca03c5a58658ad884cc6fbd

```
which means that we will use the library in the repo above with that specific
commit.
As for the reason, Mr. Allen says that currently, supporting `persistent-2.8.1`
is not his priority and he gives the above lines as the solution of this situation.
Thanks, Mr. Allen.

Then, we will remove `src/Add.hs` file and remove any references to it.
And that means, we will remove `/add/#Int64/#Int64 AddR` in `./routes`,
`import Add` in `src/Foundation.hs`, and `<a href=@{AddR }>` in `src/Home`

After that, we will add the following packages to the `package.yaml`

- `classy-prelude-yesod`: just what "the kool kids" use plus some nice `yesod` helpers.
- `classy-prelude`
- `esqueleto`: Who doesn't love joining tables?
- `yesod-auth-hashdb`: We really need authentication.
- `yesod-auth` 
- `yesod-core`: Core framework.
- `yesod-form`: We will use forms extensively here.
- `yesod`

Now, let's push it to our repo. (our current progress is saved
[here](https://gitlab.com/ibnuda/Cirkeltrek/commit/fdd5220cce105a2f75fa9d9403b892b4da18c534))

### Foundation Building

In the scaffolded templates, you will see a lot of stuff going, on which baffled
me a couple months ago, from settings, database, and templating.
Surely, the yesod-book and the templates' comments helped me a lot but it was 
not enough for to grasp the reasoning of the decisions made by the author of the
templates.
That and my hot-headed temper combined, resulted a few abandoned projects in my
GitLab account.
I'm sorry for ranting.

Okay, so we will continue the this little toy.
In yesod, when one wants to create an instance of `Yesod` he should start by defining
the routes.
As you can see in the `Yesod.Core`'s haddock, `Yesod site` has signature (or something)
of `class RenderRoute site => Yesod site`.
Which in turn, `RenderRoute site` has signature of `class Eq (Route site) => RenderRoute site`.

Let's talk about `class`es first.
In short, there's a thing in Haskell called `typeclass`.
I'll just give you an analog, let's say that  you have a kid and you want to let
him join a gifted class.
There are a few requirements for that, of course.
For example, he has grades that rise steadily and younger than his peers.
So, in Haskell, if you want to let your data recognised as one of the classes, you
have to fulfill the requirements (or "minimal complete definitions") of that class.

So, in order to make a `Yesod App`, we should start by defining the route first.
Therefore, in `src/Foundation.hs`, we will modify it into the following
```
{-# LANGUAGE QuasiQuotes #-} -- needed for parseRoute quotation
data App = App
mkYesodData
  "App"
  [parseRoutes|
    / HomeR GET
  |]

instance Yesod App
```
That `App` above is the application we are building.
We surely can name it anything, given doesn't clash with any other things, if we want.
And followed by `mkYesodData` which takes a string (which is the name of our site
application) and a `Q`uasi quotation of our routes.
Finally, we will make our site application as an instance of `Yesod` so it will
be able to serve the requests to `http://localhost:3000/`.

Also, because `App` is a record, we can fill it with whatever we need.
In the scaffolded templates, there are a few wrapped data.
For example, there is an `AppSettings`, `ConnectionPool`, and a few other data.

Let's start by adding a `ApplicationSettings` and `ConnectionPool` into `App` first.
```
data App = App
  { appSettings::ApplicationSettings
  , appConnectionPool :: ConnectionPool -- from "persistent", module Database.Persist.Sql
  , appLogger :: Logger -- from module Yesod.Core.Types
  , appStatic :: Static -- from "yesod-static", module Yesod.Static
  , appHttpManager :: Manager -- from "http-client", module Network.HTTP.Client
  }

```
which begets a need of datatype named `ApplicationSettings`.
So, we will create a file named `Settings.hs` in the `src` directory and fill it
with the settings we need for this clone.

```
data ApplicationSettings = ApplicationSettings
  { appStaticDir :: String
  , appRoot :: Maybe Text
  , appDBConf :: PostgresConf -- from "persistent-postgresql" package, module Database.Persist.Postgresql
  , appHost :: HostPreferences -- from "warp" package, module Network.Wai.Handler.Warp
  , appPort :: Int
  , appReloadTemplate :: Bool
  , appMutableStatic :: Bool
  , appSkipCombining :: Bool
  , appDetailedRequestLogging :: Bool
  }

```
At the snippet above, we wrapped a few configurations from database configuration,
port that we are going to use, and a few other things.
Do we need it?
Surely we need it for the ease of development process like in the scaffolded templates. 
Other than to ease the development process, we surely want to set some configurations
that meet our needs.
Example given, we will use `appDBConf` field to create a `ConnectionPool` for our `App`.

Now, let's back to `Foundation.hs` and continue our instantiation of our `Yesod` app.
First, we have to import our `ApplicationSettings` in `Settings` module.
Then, we need to customise our `Yesod` instance of `App` by defining `approot`.

```
instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

```
It's just our usual function. "If `approot` is not set, set it by guessing approot
based on the request and application. Else? Great! We'll use that!"
Unfortunately, when we try to compile it, the compiler will fail to do so because
"There's no instance of Yesod dispatch" or something like that.
Basically, `app/Main.hs` cannot be compiled because we have modified `App` to take
an `ApplicationSettings`.
Surely we remember that we have wrapped `ApplicationSettings` in an `App`, right?

So, let's fix that.
As written in yesod-book, we are using `warp` as our web server.
Among many functions to run `warp` in its hackage page, all of them take `Application`
(Wai's web application) as one of their inputs.
Based on our situation, where we have a yesod instance and the need for database,
we have to transform our `App` into `Application` and then run in using one of `warp`
runner functions.

Let's head to `src/Application.hs` to define our new `main` function.
Here, we will run the transformed `App` on warp using our defined settings which
was defined in `ApplicationSettings` using `runSettings`.
Why do we use `runSettings`, because we want `warp` to run at our defined port, host,
etc.

Now, will create a function to create a warp `Settings`  from our `ApplicationSettings`.
```
warpSettings :: App -> Settings -- from module Network.Wai.Handler.Warp
warpSettings app =
  setPort (appPost $ appSettings app) $ -- 1
  setHost (appHost $ appSettings app) $ -- 2
  setOnException
    (\_req exception -> 
      when (defaultShouldDisplayException exception) $
  --- ^ from Control.Monad    ^ from Network.Wai.Handler.Warp
      messageLoggerSource
        app
        (appLogger app) -- 3
        $(qLocation >>= liftLoc) -- from "template-haskell", module Language.Haskell.TH.Syntax
        "yesod"
        LevelError
        (toLogStr $ "Exception from warp" ++ show e))
    defaultSettings -- from Network.Wai.Handler.Warp

```
Basically, the function above creates warp's `Settings` from our `App`.
As you can see, there are three (1, 2, 3) used settings there. 

Now, let's make a Wai `Application` from our `App`.

```
makeApplication app = do
  commonapp <- toWaiApp app
  return $ defaultMiddlewaresNoLogging commonapp

```
I guess the function above is pretty clear.
And now, we have transformed our `App` into a Wai `Application`.
So let's run Warp server using two snippets above.

```
newMain = do
  app ???
  commonapp <- makeApplication app
  runSettings (warpSettings app) commonapp

```
Okay, I forgot where do we get an `App`.

But remember, in order to have an `App`, we have to have:

- a `Logger`, which can be easily had from `Yesod` instantiation.
- a `ConnectionPool`, which can be easily had by creating `ConnectionPool` from `PostgresConf`
- an `ApplicationSettings`. Surely we can hard code it, but I want to read it from
  an external file. Other than it's easier to remember, I want to make sure
  that there's a single source of truth.

At this point, our progress can be viewed
[here](https://gitlab.com/ibnuda/Cirkeltrek/commit/71a094349e2e4a37dc0c698709a161ff33cb0dd9)

So, let's make an `App` by creating it from an `ApplicationSettings`.

```
{-# LANGUAGE RecordWildCards #-}
makeFoundation :: ApplicationSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager -- from package "http-client-tls", module Network.HTTP.Client.TLS
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger -- from "fast-logger", module System.Log.FastLogger
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel -- from "yesod-static", module Yesod.Static
       else static) -- from "yesod-static", module Yesod.Static
      (appStaticDir appSettings)
  let mkFoundation appConnectionPool = App {..} -- RecordWildCards
      tempFoundation =
        mkFoundation $ error "Connection pool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <-
    flip runLoggingT logFunc $
    createPostgresqlPool -- from "persistent-postgresql", module Database.Persist.Postgresql
      (pgConnStr $ appDatabaseConf appSettings)
      (pgPoolSize $ appDatabaseConf appSettings)
  return $ mkFoundation pool

```
You see, we can create an `App` by creating its components and using `RecordWildCards`.
As copied above, first we create a `appHttpManager` by calling `getGlobalManager`.
It is strongly recommended to use a single keep-alive connections tracker in a web
application like this.
And that is why we call the `global` thing.
Next, just our standard output logger.
You can change it to a file if that suits your taste.
For creating `appStatic`, which is used to serve the static files, we can do it by
checking the `appMutableStatic` from `appSettings`.
The interesting part is the `ConnectionPool` creation.
We haven't have a logger, yet, while we need it in order to create a connection pool.
So, the templates create an `App` without `ConnectionPool`, though I usually choose
to use `runNoLoggingT` when creating a connection pool.

Now, we have an `App`, but we haven't an `ApplicationSettings` yet. So we will
create it by reading a config file.
Before that, we will go back to `src/Settings.hs` to create yaml decoder for our
settings.

```
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

instance FromJSON ApplicationSettings where -- package "aeson", module Data.Aeson
  parseJSON =
    withObject "ApplicationSettings" $ \ob -> do
      let defEnv = True
      appStaticDir <- ob .: "static-dir"
      appRoot <- ob .:? "app-root"
      appHost <- fromString <$> ob .: "app-host"
      appDatabaseConf <- ob .: "database-conf"
      appPort <- ob .: "app-port"
      dev <- ob .: "development" .!= defEnv
      appReloadTemplate <- ob .:? "reload-template" .!= dev
      appMutableStatic <- ob .:? "mutable-static" .!= dev
      appSkipCombining <- ob .:? "skip-combining" .!= dev
      appDetailedRequestLogging <- ob .:? "detailed-req-log" .!= dev
      return ApplicationSettings {..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml) -- package "file-embed", module Data.Embed

configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id $ decodeEither' configSettingsYmlBS -- package "yaml", module Data.Yaml

```
You see, `FromJSON` is a class of type that can be converted from JSON to a haskell type.
So, we created that instance for our `ApplicationSettings` by parsing a few yaml fields.
Nevermind about `.:` though.
It's just a fancy way to extract the field value from field name that matches the
string in the quotes.
Oh, and `defEnv`, for now we will keep it as `True`.

The other two functions?
`configSettingsYmlBS` is just a helper function from "file-embed" package that
reads a file using a magic named `TemplateHaskell` by parsing `configSettingsYaml`
into filepath `config/settings.yml`. Same with `configSettingsYmlValue`, it is a
helper function that throws an `Exception` if we can't parse the file at `src/settings.yml`.

Now, when you compile this project and run it, you will get an error about there's
no file in `config/settings.yml`.
So, just create it and fill the data.
And if you don't want to wonder how should you write it, just check the following
[commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/b5892ef5e474d8baec6cd98d3a7554d0c3de1b8a).
