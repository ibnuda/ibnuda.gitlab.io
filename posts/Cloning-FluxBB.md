Cloning FluxBB
2018-04-14 16:38:19.675105163 UTC
# Cloning FluxBB (In Progress)
 
Writing a forum software is one of a few things that I-wished-I-have-but-haven't
in the last of couple of years.
For example, among a gazillion of abandoned repos on this GitLab account,
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
Not only that, a lot of its fields are `nullable`, so we can remove them in this
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

Furthermore, we will not use some of FluxBB's tables.
For example, we won't use `permissions`, `forum_subscribtions`, and `topic_subscribtions`.

### Inner Workings

In real FluxBB's use case, an administrator could set a set of permissions for
a certain group (`moderator`, for example) to do something peculiar, like capable
of banning users, but not editing users' profiles.
We will not give the users and administrator these kind of features because it's too
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
That means, we will treat this forum's interface (HTML) purely as a document presentation.
Baring a few buttons and text inputs.
I really hope you don't mind about it.

### The Approach

We will build up the forum from `yesod-minimal` template using stack and then slowly
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

#### Writing Foundation

In the scaffolded templates, you will see a lot of stuff going on, which baffled
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
I'll just give you an analogue, let's say that you have a kid and you want to let
him join in a gifted class.
There are a few requirements for that, of course.
For example, he has grades that rise steadily and younger than his peers.
So, in Haskell, if you want to let your data to be recognised as one of the classes,
you have to fulfill the requirements (or "minimal complete definitions") of that class.

In order to make a `Yesod App`, we should start by defining the route first.
I mean, how would the web server know what kind of content if it doesn't know
anything.
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
application) and a `Q`uasi-quoted routes.
Finally, we will make our site application as an instance of `Yesod` so it will
be able to serve the requests to `http://localhost:3000/`.

Also, because `App` is a record, we can fill it with whatever we need.
In the scaffolded templates, there are a few wrapped data.
For example, there is an `AppSettings`, `ConnectionPool`, and a few other data.

Let's start by adding a `ApplicationSettings`, `ConnectionPool` and a lot of other
stuff into `App` first. Don't worry, we will talk about it later.
```
data App = App
  { appSettings :: ApplicationSettings
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

This time, when you run this project and direct your browser to [localhost:3000](http://localhost:3000)
you will get a page with **Nice** written on it.

We will continue building our `App` so it has nice things we need (or want).

Let's start with creating a shorthand for `Form`.
That type usually really mouthful to write.
So, instead of `Html -> MForm (HandlerFor App) (FormResult a, Widget)`, we just
going to use `Form a`.
```
type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)

```
and for database thingy,
```
type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a

```
Ah, yes. There's also a keyword `forall`.
Whenever you see a `forall`, just think that you argument for the argument will
be used in the signature.
In this case, `m`, which is a function that transform anything into any other thing,
will be used as the value for `MonadIO`.
And let's not talk about `MonadIO`. There are too many shit stirred because of `Monad`
and too few articles about building things.

Now, have aliases and/or shortcuts, so let's continue fulfilling our `Yesod`
class requirements.
```
{-# LANGUAGE TemplateHaskell #-}
instance Yesod App where
  -- cont.
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmessage <- getMessage -- module Yesod.Core
    mcurrentroute <- getCurrentRoute -- module.Yesod.Core
    pagecontent <- widgetToPageContent $ do -- module Yesod.Core
      [whamlet| -- Template haskell.
        $maybe route <- mcurrentroute
          <p> You're at #{show route}.
        $nothing
          <p> You're lost.
        ^{widget}
      |]
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet") -- from package `sakespeare`, module Text.Hamlet

```
Here, we defined two new functions.
`yesodMiddleware`, which we didn't actually change it because `yesodMiddleware`'s
default is `defaultYesodMiddleware`.
Heh, talk about futile endeavor.
But, if you read the doc, middleware is something like a gatekeeper of requests.
Each and every request will be checked, modified, or even rejected by the
middleware if it doesn't exceed middleware's expectation.
The other function is `defaultLayout`.
This function takes a `WidgetFor` and transform it into `HandlerFor App Html`.
Basically, this function will be used as the default layout when you didn't
specifically define what the web server should use.
If you don't believe me, run this executable, and direct your browser to
[undefined route](http://localhost:3000/youwillnotfindanythinghere) and you will
still see `You're lost.` which was defined above.
As for how would we write inside of `[whamlet| _ |]`, pretty much it's
a simplified html.
But instead of wrapping things with `<element>__</element>`, just give it an
indentation.
Yesod book has a detailed explanation about it.
Oh yeah, don't forget to create a directory named `templates` and a file in it
named `wrapper.hamlet`.

You can see the progress of this project [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/ec1bf4968e06f43fe10aacb076a327571a4bceb1).

#### Modeling Database

Now, we will write the database's representation using `persistent` package.
First, we should create a file named `Model.hs` in `src` directory.
Though the actual representation is pretty long for this blog post, we will only
include two or three tables in this post.
```
-- skipped a ton of extensions.
module Model where
import ClassyPrelude.Yesod
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Topics
      forumId ForumsId
      poster Text
      subject Text
      repliesCount Int default=0
      startTime UTCTime
      lastPost UTCTime Maybe
      lastPostId PostsId Maybe
      lastPoster Text Maybe
      isLocked Bool default=false
      deriving Show Eq
    Posts
      topicId TopicsId
      number Int
      username Text
      userId UsersId
      time UTCTime
      content Text
      deriving Show Eq
    Users
      groupId GroupsId
      username Text
      email Text
      password Text Maybe
      joinTime UTCTime
      topicsStarted Int default=0
      repliesPosted Int default=0
      UniqueUsername username
      UniqueEmail email
      deriving Show Eq
  |]

```
You see, I've written a few basic concepts of `persistent` in [here](2017-11-03-authorization-in-servant.html) you can check it out.
If you see closely, one of `Users`'s block, `password` field is a `nullable`.
We let it as a `nullable` because package `yesod-auth-hashdb` needs it.
Don't worry, we'll check it out later.

If you re-open FluxBB's database structure, you will know how different our
models and theirs.
Mostly, FluxBB doesn't use foreign keys and a lot of nullables.
Here, in our model, we don't use the nullables that much and only when we really
have to use it.
Other than to reduce noises, we don't want to write too much, right?

Now, we have defined the database representation.
We will continue spending our leisure time by integrating the previous module
to our `Foundation`.
So, let's back to `src/Foundation.hs`.

There, we will make sure our `App` is in `YesodPersist` class.

```
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnectionPool master

```
By fulfilling `YesodPersist`'s class qualification, we can talk to the database
in our web application.
You see that `runSqlPool` line?
That's how the query being executed.
And executing the query in our web application?
Easy, just `runDB query`!
Though you have to add `liftHandler` in front of `runDB query` with the reason
which will be talked later.
Promise!

Now, the nice thing is we will let our web application to create its needed
tables by itself.
Let's head to `src/Application.hs` and add a few lines of function.

```
import Model
makeFoundation = do
   -- skip
   pool <- --skip
   runLogginT (runSqlPool (runMigration migrateAll) pool) logFunc -- added
   return $ mkFoundation pool
```
The added line means that we will run the migration plan `migrateAll` which was
defined at the `Model.hs`, inside the `share`'s first arguments.
Now, when you compile this project and run it, you will see something like the
following 
```
Migrating: CREATe TABLE "categories"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL)
Migrating: CREATe TABLE "forums"("id" SERIAL8  PRIMARY KEY UNIQUE,"category_id" INT8 NOT NULL,"name" VARCHAR NOT NULL,"descriptions" VARCHAR NULL,"topics_count" INT8 NOT NULL DEFAULT 0,"replies_count" INT8 NOT NULL DEFAULT 0,"last_post" TIMESTAMP WITH TIME ZONE NULL,"last_post_id" INT8 NULL,"last_poster" VARCHAR NULL)
-- etc.

```
You can see the progress to this point by looking at this [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/fe886b3bed75c8089e671d0f4fd2fa17c0ac8d59).

#### Niceties

Well, it's true that stopping `stack exec`, running `stack build`, and then
running `stack exec` again is tiring.
So, we will implement scaffolded templates' `stack exec -- yesod devel` at
`src/Application.hs`

```
{-# LANGUAGE NoImplicitPrelude #-}
import ClassyPrelude.Yesod
import Network.Wai (Middleware)

makeLogware :: App -> IO Middleware
makeLogware app = do
  mkRequestLogger
    def - package classy-prelude-yesod
    { outputFormat =
        if appDetailedRequestLogging $ appSettings app
          then Detailed True
          else Apache FromFallback
    , destination = Logger $ loggerSet $ appLogger app
    }

makeApplication app = do
  commonapp <- toWaiApp app
  logware <- makeLogware app
  return $ logware $ defaultMiddlewaresNoLogging commonapp

```
Why would we modify this function, you say.
"Because we want to have nice formatted log request," that's why.
Anyway, basically the function above checks whether we want a detailed request
or not and serves accordingly.
Then, we will feed it into our application.
Remember, our application is a Warp Application, so we can chain as many
middleware as we want.
Provided we can supply it, of course.

Okay, let's continue adding niceties in `Application`.
Let's start by creating a function that reads settins.
```
getAppSettings :: IO ApplicationSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv
```
That type signature above, that's important because `loadYamlSettings` wants to
read any yaml file that can be parsed into Haskell object.
So, we have to add that signature.
And why `IO`? Because we read it from RealWorld(tm), my friend.

Now, we continue it by writing a function that returns a tuple of Warp `Settings` and our `Application`.
Simple, actually.
```
getAppDev :: IO (Settings, Application)
getAppDev = do
  settings <- getAppSettings
  found <- makeFoundation settings
  warpsettings <- getDevSettings $ warpSettings found
  app <- makeApplication found
  return (warpsettings, app)

develMain = develMainHelper getAppDev

```
We just do the same as our `newMain`, but instead of returning an `Application`,
we add another value named `warpsettings`.
Surely both of the return value will be used by `develMainHelper` to run it as
long as we want.

After you save all of the modified files, now go to your favorite terminal
emulator and run `stack exec -- yesod devel`.
Now, whenever you modify anything under `src/` directory, yesod will recompile
the modified module and anything that depends on it.

Check our progress [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/f3d72f0a11ec786374a8dc44803cbf49854d5684).

#### Authentication, Prelude

We are, approximately, has 70% our base template, minus handlers.
So, we will add another few percents of it by adding authentication feature.

First, we will go back to `src/Model.hs` just to add three lines of class
instantiation of `HashDBUser`.
It's simple enough, really.
```
instance HashDBUser Users where
  userPasswordHash = usersPassword
  setPasswordHash u h = u {usersPassword = Just h}

```
Basically we just tell that `Users` is an instance of `HashDBUser` by defining
those two functions.
First, we tell that `userPasswordHash` should just use `usersPassword`.
And `setPasswordHash` just a setter for `Users`.

The next part is also a bit simple.
We will head to `src/Foundation.hs` to make our `App` be able to authenticate
our users.
```
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = UsersId
  loginDest _ = HomeR
  logoutDest _ = LoginR
  redirectToReferer _ = False
  authPlugins _ = [authHashDB (Just . UniqueUsername)]
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUsername $ credsIdent creds
    case x of
      Nothing             -> return $ UserError InvalidLogin
      Just (Entity uid _) -> return $ Authenticated uid

instance YesodAuthPersist App

```
You know why we should define our `App` to has a `RenderMessage` instance?
Because `YesodAuth` is something like a sub class of `RenderMessage`.
Just think of `YesodAuth` is "top rankers of 10th graders".
And in order to be a "top ranker of 10th grader", your kid should be a 10th
grader in the first place.
`RenderMessage` is analog to the "10th graders" in this context.

Now, to make our `App` capable of authenticating our users, we defined
`YesodAuth` instance for our `App` and its requirements.
`AuthId App` type above was specifically defined to differentiate the session
of the access to our web by reading `UsersId` in the cookies (or something like that).
`loginDest` and `logoutDest` handle what should a user see after he logged
in and logged out to/from his session.
while `redirectToReferer` determines whether should a user be redirected
to the `loginDest` or not.

The core of our authentication process lays at these two functions
 - `authenticate` which basically reads `credsIdent` and compares the result from 
   our database.
 - `authPlugins`, just like its name, it defines what should we use in order
   to authenticate our users.
   And in this case, we use `authHashDB` from username.
   Why? because that's what matter.

But wait, we can't be sure whether we can really be authenticated or not, right?
That's pretty disappointing, actually.
It's time to start writing `Seed` program where it seeds our database population.

Let's create a directory name `seed` and create a file named `Main.hs` and modify
`package.yaml` to include `seed` as one of our program.
```
executables:
  -- skip
  Seed:
    main: Main.hs
    source-dirs: seed
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - our-project

```
Which basically tells stack (or is it cabal? or is it hpack?) that there is
a new entry for the executables named `Seed` with the entry point in `Main.hs`
which located in `seed` directory.

What left is what should we write for it.
```
insertGroup txt = insert $ Groups txt

createAdministrator gid username email password = do
  now <- liftIO getCurrentTime
  let usersGroupId = gid
      usersUsername = username
      usersEmail = email
      usersPassword = password
      usersJoinTime = now
      usersTopicsStarted = 0
      usersRepliesPosted = 0
  insert_ $ Users {..}

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv -- [1]
  let conn = (pgConnStr $ appDatabaseConf settings) -- [2]
  username <- getLine -- [3]
  password <- -- [4]
    do something <- getLine
       password <- makePassword (encodeUtf8 something) 17
       return $ decodeUtf8 password
  email <- getLine -- [5]
  runStderrLoggingT . withPostgresqlConn conn $ -- [6]
    runSqlConn $ do
      runMigration migrateAll
      gid <- insertGroup Administrator
      gin <- insertGroup Moderator
      gim <- insertGroup Member
      gib <- insertGroup Banned
      createAdministrator gid username email (Just password) -- [7]

```
There are a few functions here.

- `insertGroup` which takes a `Grouping` from `src/Model/Grouping.hs`.
  It's just a standard `persistent`'s insert function, to be honest.
  You put a datum as an argument for `insert`, and then when you're ready,
  you will execute it.
- `createAdministrator` which takes a few arguments which their datatypes
  satisfy our `Users`'s fields' datatypes.
  If you can't remember what are they, you can look up at the `Model.hs`
  snippet above.
- `main`, our main program.
  What it does:
  
  1. Reads the config file.
     You can also the same function usage in `Application.hs`
  2. Creates a connection settings.
  3. Reads username from command line input.
  4. Creates a password, which reads from cli input, make the password
     out of it, and then returns it.
  5. Reads email, though the input's validity won't be checked.
  6. Executes the two previous functions.
  7. Creates an administrator from our read inputs.

Now, when we compile our project (`stack build`), we will have another binary
executable named `Seed`.
When we execute it (`stack exec Seed`), it will ask our needed inputs.
Just fill it casually.
When you have run it, look at the database (`select * from users`) and you will
see that our data inputted there.

Progress til now: [this](https://gitlab.com/ibnuda/Cirkeltrek/commit/8525a8c28475025afbb6566a92df04a6bd2ad61f)

#### Authentication, Login & Logout

Now, since our supporting infrastructure is satisfied, then the next step is to create
where should the user head to log in, right?
That's why we will add a route to our application and its handler and modify `defaultLayout`.

```
-- Foundation.hs
mkYesodData "App"
  [parseRoutes|
    /        HomeR     GET
    /auth    SigninR   Auth getAuth
    /profile ProfileR  GET
  |]
  
```
You see that `/profile ...` line above?
That's our new route.
Don't worry to see it, because the program won't compile because it doesn't have
the required handle.
```
-- Profile.hs
getProfileR :: Handler Html
getProfileR = do
  (Just (Entity userid user)) <- maybeAuth
  defaultLayout $ do
    setTitle "Nice"
    [whamlet|
      <p> You are: #{usersUsername user}
      <p> Userid: #{fromSqlKey userid}
      <p> Your email: #{usersEmail user}
    |]

```
In case you're wondering why would we "extract" `maybeAuth` directly as `Just x`,
we are going to make `ProfileR` as a protected route where only logged in users
could see it.
And in order to do that, we have to modify `Foundation.hs` to be able to "guard"
the route above.

```
instance Yesod App where
    -- snip
  authRoute _ = Just $ SigninR LoginR -- [1]
  defaultLayout widget = do
    master <- getYesod
    maut <- maybeAuth -- [2]
    mmessage <- getMessage
    pagecontent <- widgetToPageContent $ do
      [whamlet|
        $maybe aut <- maut
          <a href=@{SigninR LogoutR}> Logout -- [3]
        $nothing
          <a href=@{SigninR LoginR}> Login -- [3]
        ^{widget}
      |]
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
  isAuthorized (SigninR _) _ = return Authorized -- [4]
  isAuthorized HomeR _       = return Authorized -- [4]
  isAuthorized ProfileR _    = isLoggedIn -- [4]

isLoggedIn :: Handler AuthResult
isLoggedIn = do -- [5]
  maut <- maybeAuth
  case maut of
    Nothing -> return $ Unauthorized "login please"
    Just _  -> return Authorized

```
At the snippet the interesting parts are the marked ones.

1. We stated that there is a login / auth route.
2. We put this line in order to get the "auth status" of the incoming request.
3. We used the point 2's result to determine whether we should display login link
   or logout link.
4. This is where the app guards the routes.
   We specifically defined that only requests that satisfy `isLoggedIn` to
   access `ProfileR`.
5. `isLoggedIn` basically checks the requests whether it has auth in the session cookies
   or not.
   If the requests don't have it, then we can just short circuit the process by returning
   "Unauthorized access".
   Otherwise, we will continue to process the requests.
   
Now, we should add `import Profile` to `src/Application.hs` and then run it.
We will be able to login and logout to and from our application.
Nice.

You can check out the our progress [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/2c11ea6404e72e868aea7584c396806c1c5f913f).

#### Default Layout

Considering how long HTML is, I think we should separate it from our code.
What I mean by separation is, the program should just read the hamlet files
and return the templates.

Now, let's head for `Settings.hs`, where everything related to settings are there.

```
compileTimeAppSettings :: ApplicationSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e   -> error e
    Success s -> s

widgetFile :: String -> Q Exp
widgetFile =
  (if appReloadTemplate compileTimeAppSettings
     then widgetFileReload
     else widgetFileNoReload)
    widgetFileSettings
  where
    widgetFileSettings :: WidgetFileSettings
    widgetFileSettings = def

```
Those two functions above are used in the scaffolded templates to load (and/or reload)
the template ([hamlet, julius, and casius](https://www.yesodweb.com/book/shakespearean-templates)) files.
About `compileTimeAppSettings`, it just reads the setting value at the compile time and
throws an error when there's no valid settings.
Though it's pretty much impossible to throw error because `configSettingsYmlValue` will
throw error first if the setting file has invalid values.
While `widgetFile` just reload (or not) the templates based on the settings.

So, what should we do next is applying `widgetFile` function to our application.
And it's actually pretty simple.
```
instance Yesod App where
  defaultLayout widget = do
    ---
    pagecontent <- widgetToPageContent $ $(widgetFile "def") -- this one.
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
```
And if you wonder why do we use `widgetFile` while `hamletFile` does exist,
if you look at `hamletFile` signature, it returns `Html` while what
we need is `WidgetFor App ()`

I will hereafter always skip hamlet contents for the sake of cutting the
word counts down.
You can still see the commited files at the of sub-sub-sub-section as usual
though.

And now, when load our site in the browser and see its source, you will
see the structure of our templates.

Here's current progress [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/f30a18c656a62574ae160445a6c2b093ba8ee607).
Also, the page looks weird without any css, we will add that by adding
static route in our app.

```
-- Foundation.hs
mkYesodData "App" [parseRoutes|.... /static StaticR Static appStatic ....]

```
and create a file named `StaticFiles.hs` at a new directory `Settings` which
contains
```
-- skip extensions and imports
staticFiles (appStaticDir compileTimeAppSettings)

```
Surely we know what the first snippet does.
As it just parses new `/static` route.
But the second snippet, unfortunately, I'm not really sure what it does.
But surely it relates to GHC and Template Haskell restrictions.

Anyway, let's add css files to `defaultLayout`.

```
  defaultLayout widget = do
    -- skip
    pagecontent <- widgetToPageContent $ do
      addStylesheet $ StaticR css_main_css
      addStylesheet $ StaticR css_milligram_min_css
      addStylesheet $ StaticR css_main_css
      $(widgetFile "def")
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")

```
And then reload our site and you'll see that our css is applies to the
layout.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/f21104e9cabd3c52f10c9c0b37bd906b582e9829).

##### Login Layout

I don't know, my friend.
I dislike the default layout of the login form by `yesod-auth-hashdb`.
So we will fix that by modifying `YesodAuth` instance, `authPlugins` function to
```
  authPlugins _ = [authHashDBWithForm loginform (Just . UniqueUsername)]
    where
      loginform :: Route App -> Widget
      loginform action = $(whamletFile "templates/login.hamlet")

```
The difference between `authHashDB` and `authHashDBWithForm` is that the later
receives an addition argument of `Route App -> Widget`.
And we provided a function that exactly has that kind of signature.
Don't worry about the content of `templates/login.hamlet`.
I will include it in the next commit.

#### Completion of Foundation

Now that we have the needed foundation that provides:

- Basic routes, which has protected content, authentication, and public content.
- Layouts.

The next part is where we clone (most of) the functionalities of FluxBB.

[commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/143696db00a6781cc4081c335c8069b2b5f197d2).


### Cloning Things

Here, we will try to copy the functionality of FluxBB.
Although it won't 100% parity

#### Preparations

Before we do anything, let's start by cleaning up our imports.
For example, instead of importing `Yesod.Core`, `ClassyPrelude.Yesod`, etc on
each and every file, we can import a single file that has exported other modules.

```
-- src/Import/NoFoundation.hs
module Import.NoFoundation where
import Yesod.Auth as Import
import ClassyPrelude.Yesod as Import
-- import anything that you want to import.

-- src/Import.hs
module Import where
import Import.NoFoundation as Import
import Foundation as Import

```

Now, whenever you see a module that imports, say, `Yesod.Auth`, you can simply
replace it with `import Import`.
What we just did was to simplify our imports by "combining" our modules into
a single module.

And create a directory named `Handler` to contain every handler we have.
Plus, don't forget to update the name of the module that responsible for handling
requests (`Home` and `Profile`).
The purpose of this move is to ensure that we separate our (modules') concerns.
Other than that, I dislike a disastrous `~/` and any root project is.

Now, we have cleaned our root project directory, let's start.

#### Category Administration

If you're wondering why do we start with administering category, then I will
confess that I feel I can solve this problem better when I approach it top-down.
It doesn't mean that you should the same, but, I don't know.
Just like Sinatra has sung, I will just do it in my way~

Okay, administering category in this context actually just means that we can create
categories and delete them, if the situation demands us to do so.

Based on the requirements above, we should:

- Ensure that only `Administrator` that could create and/or delete categories.
- Ensure that the forums of a category should be also deleted when we delete its
  parent category.

Therefore, we should create functions that satisfy the requirements above.

##### Category Creation: Business Logic and Database Query

First, we should look at the `Categories` entity at `Model.hs`.
There, it was clear that a `Categories` only have a field named `categoriesName`
which should be come from our `Administrator`.
So, we should have a function that takes two parameters, `grouping` and `catname`
and should return an `OK` when our input pass our requirements and `Permission Denied`
when it doesn't pass.

So, let's create a directory named `Flux`, a file named `AdmCategory.hs` in it, and
fill it with
```
module Flux.AdmCategory where
import Import

createCategory Administrator catname = liftHandler $ runDB $ _ catname -- [1]
createCategory _ _ = permissionDenied "You're not allowed to do it." -- [2]

```
The function above is just a standard pattern matching function.
[2] will just throw a permission denied when the one who asks for
The interesting part is at [1], there's a "holed" parameter.
When you compile it GHC will scream about `ReaderT (YesodPersistBackend (HandlerSite m)) (HandlerFor (HandlerSite m)) a`.
That's where we should create the query.

Now, let's create a directory named `DBOp`, a file named `CRUDCategory.hs` in it, and
fill it with
```
module DBOp.CRUDCategory where
import Import

insertCategory catname = insert $ Categories catname

```
And modify `Flux.AdmCategory` to import `DBOp.CRUDCategory` and replace the "holed" (`_`) function
with `insertCategory`.

Surely there are a few complains from GHC.
Don't worry, if you use intero, just apply the suggestion (C-c C-r C-c C-c) by
adding `GADTs` extension to it.

And that's it.
Basically we have completed the logic of this part.

##### Category Creation: Route and UI

In this page, a FluxBB administrator would see a page filled with something like the following
(which was dumbed down a lot):
```
├── New Group
│     Group Name [ input ]
│       [button create]
│
├── Group List
│     Groups [ dropdown ]
│       [button delete]
└──

```

Which means we should have two parts of input group.
The first part is a form which takes a textfield for a category name and the second one
is a dropdown (or select?) to display the list of the existing groups (to be deleted).

Now, to simplify a lot of things, we will create the data form for those two forms
in the same file for the handler function of this route.

```
-- src/Handler/Adm/Category.hs
module Handler.Adm.Category where
-- skip imports (Imports, Database.Esqueleto, Flux.AdmCategory)
data CreateCategoryForm = CreateCategoryForm { createCategoryFormName :: Text } deriving (Show)

createCategoryForm :: Form CreateCategoryForm
createCategoryForm = renderDivs $ CreateCategoryForm <$> areq textField "Category Name" Nothing

data SelectCategoryForm = SelectCategoryForm { selectCategoryFormId :: Int64 } deriving (Show)

selectCategoryForm :: [Entity Categories] -> Form SelectCategoryForm
selectCategoryForm cats = renderDivs $ SelectCategoryForm <$> areq (selectFieldList catlist) "Category" Nothing
  where
    catlist = map (\(Entity cid (Categories name)) -> (name, fromSqlKey $ cid)) cats :: [(Text, Int64)]

getAdmCategoryR :: Handler Html
getAdmCategoryR = do
  (widc, enctc) <- generateFormPost createCategoryForm
  defaultLayout
    [whamlet|
      <form enctype=#{enctc}>
        ^{widc}
        <input .button-primary value=create type=submit>
    |]

```

Form data above is just our usual yesod's form. Nothing special there.
But when you see `selectCategoryForm`, you will see that it takes a list of `Entity Category`
We will use that to create a dropdown list that contains `Groups`' names and `Groups`' ids.
And then we will use `createCategoryForm` to generate a post-form which also be defined later.

The next step is modifying our `Foundation` to support the route and miscelanii.

If you were wondering where did we get the `grouping` of a request, we will create that.
Let's head to `Foundation.hs`, import `Database.Esqueleto`, hiding a few operators,
and then add a function there.
```
mkYesodData
  "App"
  [parseRoutes|
    -- skip
    /admin/category  AdmCategoryR GET
  |]

getUserAndGrouping :: Handler (Maybe (Key Users, Text, Grouping))
getUserAndGrouping = do
  maut <- maybeAuth
  case maut of
    Nothing -> return Nothing
    Just (Entity uid user) -> do
      [gro] <-
        liftHandler $
        runDB $
        select $
        from $ \(group, user) -> do
          where_
            (user ^. UsersId ==. val uid
             &&. group ^. GroupsId ==. user ^. UsersGroupId)
          limit 1
          return (group ^. GroupsGrouping)
      return $ Just (uid, usersUsername user, unValue gro)

```
The function above retrieves from the following columns: `users.id`, `users.username`, and
`groups.grouping` by doing the following things:

1. Gets the authentication status of a request.
2. In case of the absence of authenticated status, it returns nothing.
3. Otherwise, it queries the database what is the `Grouping` of a user
   with id `uid`.
4. Returns the group and and wrap them into a triple.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/be2dd0d1f592a80cf11c36b487d109125038eeb6)
And I'm sorry for commiting so often.
I'm afraid commiting mistakes when writing this article.
Especially about the flow of the software-writing process.

When we load localhost:3000/admin/category in our browser, we will see a page with
`Permission Denied, login please` written on it.
And after we login using the user that we have created previously using `Seed` program,
and head the same page, we will be greeted by textfield with "Group name" written
on top of it.

Now, let's integrate `getUserAndGrouping` in route admin.
```
allowedToAdmin :: Handler (Key Users, Text, Grouping)
allowedToAdmin = do
  midnamegroup <- getUserAndGrouping
  case midnamegroup of
    Nothing -> permissionDenied "You're not allowed to see this page."
    (Just (uid, name, Administrator)) -> return (uid, name, Administrator)
    (Just (uid, name, _)) -> permissionDenied "You're not the admin of this site."


-- And insert it at the topmost handler.
getAdmCategoryR :: Handler Html
getAdmCategoryR = do
  (uid, name, group) <- allowedToAdmin
  -- the rest is still the same.

```
You see, when there's no session associated by `getUserAndGrouping` we can tell the user
that he is not allowed to see the page.
Even we can tell the users who aren't an `Administrator` that they're not allowed to do the same.
What nice is, because `allowedToAdmin` is a shortcircuiting function, we can be really
sure to extract `uid`, `name`, and `group` from the previous function.

If you don't believe me, you can create a `Moderator` or `Member` user directly in the database
and access this route.

```
cirkeltrek=# select grouping, username, password from users, groups where users.group_id = groups.id;
   grouping    | username  |                                    password
---------------+-----------+---------------------------------------------------------------------------------
 Administrator | iaji      | sha256|17|xxxxxxxxxxxxxxxxx1f9aQ==|2WD50Li/BMLN/nOtw9C7PALXzK4YSPgAcfnRPpRYgfU=
 Moderator     | moderator | sha256|17|xxxxxxxxxxxxxxxxx1f9aQ==|2WD50Li/BMLN/nOtw9C7PALXzK4YSPgAcfnRPpRYgfU=

```

Login with that new user, open this route, and you will be greeted by "You're not the admin of this site."
Nice.

The next part is actually do creating the new category.

```
postAdmCategoryR :: Handler Html
postAdmCategoryR = do
  (uid, name, group) <- allowedToAdmin
  ((res, _), _) <- runFormPost createCategoryForm
  case res of
    FormFailure x -> invalidArgs x
    FormSuccess r -> do
      _ <- createCategory group (createCategoryFormName r)
      redirect AdmCategoryR
    _ -> invalidArgs ["Good job, smarty pants!"]

```
That function above uses `allowedToAdmin` as a guard to keep the unauthorised users
stay away.
Then the `res` is the result of post-form parsing by `runPostForm` using `createCategoryForm`
as the "template" of input.
In case of form failure or anything that is not a succesful result, we will let them be.
Other than that, we will create the group and then redirect back to this route.

Now, try it and look the database. You will see something like the following.
```
cirkeltrek=# select * from categories;
 id |   name
----+-----------
  1 | Ayyy lmoa

```

One thing, though.
We forgot to show the list of the database so let's fix that.
In order to show all categories, we should query the database.
So, we should create the function in `DBOp.CRUDCategory`, call it
in `Flux.AdmCategory` (because of separation of concerns. LOL)
And then show the result.

```
-- DBOp/CRUDCategory.hs
selectAllCategory =
  select $
  from $ \category -> do
    orderBy [asc (category ^. CategoriesName)]
    return category

-- Flux/AdmCategory.hs
getAllCategories = liftHandler $ runDB $ selectAllCategory

--  Handler/Adm/Category.hs
getAdmCategoryR = do
  -- skip
  allcategories <- getAllCategories
  (widl, enctl) <- generateFormPost $ selectCategoryForm allcategories
  defaultLayout
    [whamlet|
      <form method=post enctype=#{enctc}>
        ^{widc}
        <input .button-primary value=create type=submit>
      <hr>
      <form method=post enctype=#{enctl}>
        ^{widl}
        <input .button-primary value=delete type=submit>
    |]

```
At the first function, I just want to show a nice DSL.
At the second function, we just "lift" the execution of the first function (which actually a query).
At the third function, which we just modified it a little, we add the dropdown for the categories
which generated from the categories in the database.
Is short, just check it out at your browser, my dude.

There's a bug, though. When you click "delete" what the system does is it inserts the key of
the shown category to as a new category.
We will fix that in the next section.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/a42e7196598b778169c984d966a6720290897687).

##### Category Deletion: Business Logic and Database Query
Please wait.

##### Category Deletion: Route and UI

Please wait.