Cloning FluxBB
2018-04-14 16:38:19.675105163 UTC
Post
 
Writing a forum software is one of a few things that I-wish-I-had-but-hadn't
in the last of couple of years.
For example, among a gazillion of abandoned repos on this GitLab account,
[`Forum`](https://gitlab.com/ibnuda/Forum) is one of them.
If you take a glance of it, you will see -- obviously -- that it's an unfinished
forum software.

So, I decided to take a different approach to finally do it, no matter what it takes.
I will use the following tech:

- FluxBB's database structure. Though in this case, it will be mutilated gruesomely.
- Yesod. A great framework with bells and whistles.
- Esqueleto. See, it's nice to be able to join tables in a bar and/or database.

Okay, before we start to write this clone, we should ensure a few things.
For example, the structure of the tables of this program and what one usually
expects when they use and/or administer a forum software.

## Database Design

Because forum software basically is just a CRUD program, we have to define the database
design first.
Fortunately, [FluxBB's database structure](https://fluxbb.org/docs/v1.5/dbstructure)
is not really complicated.
Not only that, a lot of its fields are `nullable`, so we can remove them in this
clone and make use of foreign keys constraints.

For example, we will not use some of nullables columns from `Users` table.
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
Example given, we won't use `permissions`, `forum_subscriptions`, and `topic_subscriptions`.

## Inner Workings

In real FluxBB's use case, an `administrator` could set a set of permissions for
a certain group (`moderator`, for example) to do something peculiar, like capable
of banning users, but not editing users' profiles.
We will not give the users and administrator these kind of features because it's too
complicated for my purpose.

So, instead of actual FluxBB's features, we will dumb it down like the following:

- <b>Users can</b>

  - Read `topic`s and `forum`s.
  - Create `topic` (or `thread` or `discussion`).
  - Reply `topic`.
  - Edit his own `reply`
  - Edit information.
  - Can `report` people.
  - See other users' information.

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
  
## Layout and UI

I'll make a confession, I don't understand CSS and JavaScript.
So, I will use a small CSS framework, [milligram](http://milligram.io/) as helper
and no JS in this program.
That means, we will treat this forum's interface (HTML) purely as a document presentation.
Baring a few buttons and text inputs.
I really hope you don't mind about it.

## Approach

We will build up the forum from `yesod-minimal` template using stack and then slowly
turn it into something that seems like `yesod-postgres` template and then to a FluxBB's clone.
While we're at it, I will try my best to explain why we do what we do.

# Start Typing

## Project Setup

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
which means that we will use the library at the repo's url in the snippet above on
that specific commit.
As for the reason, Mr. Allen says that currently, supporting `persistent-2.8.1`
is not his priority and he gave the above lines as the temporary solution for this
situation.
Thanks, Mr. Allen.

Then, we will remove `src/Add.hs` file and remove any references to it.
That means, we will remove `/add/#Int64/#Int64 AddR` in `./routes`,
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

## Foundation Building

### Writing Foundation

In the scaffolded templates, you will see a lot of stuff going on, which baffled
me a couple months ago, from settings, database, and templating.
Sure, the yesod-book and the templates' comments helped me a lot, but it was 
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
Nah, it's complicated.

Let's talk about `class`es first.
In short, there's a thing in Haskell called `typeclass`.
I'll just give you an analogue, let's say that you have a kid and you want to let
him join in a gifted class.
There are a few requirements for that, of course.
For example, he has grades that rise steadily and younger than his peers.
So, in Haskell, if you want to let your data to be recognised as one of the classes,
you have to fulfill the requirements (or "minimal complete definitions") of that class.

In order to make a `Yesod App`, we should start by defining the route first.
I mean, how would the web server know what kind of content it should serve if it
doesn't know anything.
At best, it will only serve you `404 Not Found`.
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
We surely can name it anything, given it doesn't clash with any other things, if
we want.
And followed by `mkYesodData` which takes a string (which is the name of our site
application) and a `Q`uasi-quoted routes.
Finally, we will make our site application as an instance of `Yesod` so it will
be able to serve the requests to `http://localhost:3000/`.

Also, because `App` is a record, we can fill it with whatever we need.
Just like in the scaffolded templates, there are a few wrapped fields.
As examples, there is an `AppSettings`, `ConnectionPool`, and a few other data.

Let's start by adding a `ApplicationSettings`, `ConnectionPool` and a lot of other
stuff into `App` first.
Don't worry, we will talk about it later.
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
  , appDatabaseConf :: PostgresConf -- from "persistent-postgresql" package, module Database.Persist.Postgresql
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
Example given, we will use `appDatabaseConf` field to create a `ConnectionPool` for our
`App`.

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
an `ApplicationSettings` and few other data.
Surely we remember that we have wrapped `ApplicationSettings` in an `App`, right?

So, let's fix that.
As written in yesod-book, we are using `warp` as our web server.
Among many functions to run `warp` in its hackage page, all of them take `Application`
(Wai's web application) as one of their parameters.
Based on our situation, where we have a yesod instance and the need for database,
we have to transform our `App` into `Application` and then run in using one of `warp`
runner functions.

Let's head to `src/Application.hs` to define our new `main` function.
Here, we will run the transformed `App` which runs on top of warp using our predefined
settings which was defined in `ApplicationSettings` using `runSettings`.
Why do we use `runSettings`, because we want `warp` to run at our defined port, host,
etc just like what we have defined.

Now, we will create a function to create a warp `Settings`  from our `ApplicationSettings`.
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
        (toLogStr $ "Exception from warp" ++ show exception))
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
- a `ConnectionPool`, which can be easily had by creating `ConnectionPool` from
  `PostgresConf`
- an `ApplicationSettings`.
  Surely we can hard code it, but I want to read it from an external file.
  Other than it's easier to remember, I want to make sure that there's a single
  source of truth.

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
We haven't had a logger yet, we need it in order to create a connection pool.
So, the templates create an `App` without `ConnectionPool`, though I usually choose
to use `runNoLoggingT` when creating a connection pool.

Now, we have an `App`, but we haven't an `ApplicationSettings` yet.
So we will create it by reading a config file.
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
It's just a fancy way to extract the field value from a field with name that matches
with the string in the quotes.
Oh, and `defEnv`, for now we will keep it as `True` because, well, it's true
that we're in a development mode.

The other two functions?
`configSettingsYmlBS` is just a helper function from "file-embed" package that
reads a file using a magic named `TemplateHaskell` by parsing `configSettingsYml`
into filepath `config/settings.yml`.
Oh yeah, we can get `configSettingsYml` by importing `Yesod.Default.Config2`.
Same with `configSettingsYmlValue`, it is a
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
And let's not talk about `MonadIO`.
There are too many shit stirred because of `Monad` and too few articles about building
things.

Now, we have aliases and/or shortcuts, so let's continue fulfilling our `Yesod`
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

### Modeling Database

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
You see, I've written a few basic concepts of `persistent` [here](2017-11-03-authorization-in-servant.html)
and if you want, you can check it out first.
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

### Niceties

Well, it's true that stopping `stack exec`, running `stack build`, and then
running `stack exec` again is tiring.
So, we will implement scaffolded templates' `stack exec -- yesod devel` feature at
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
Please give attention to type signature above, that's important because
`loadYamlSettings` wants to read any yaml file that can be parsed into Haskell object.
So, we have to add that signature.
And why `IO`? Because we read it from RealWorld(tm), my friend.

Now, we continue it by writing a function that returns a tuple of Warp `Settings`
and our `Application`.
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

### Authentication, Prelude

For now, we, approximately, have 70% feature parities compared to scaffolded templates,
minus handlers.
So, we will add another few percents of it by adding authentication feature.

First, we will go back to `src/Model.hs` just to add three lines of class
instantiation of `HashDBUser`.
It's simple enough, really.
```
instance HashDBUser Users where
  userPasswordHash = usersPassword
  setPasswordHash u h = u {usersPassword = Just h}

```
In short, we just tell that `Users` is an instance of `HashDBUser` by defining
those two functions.
First, we tell that `userPasswordHash` should just use `usersPassword`.
And `setPasswordHash` just a setter for `Users`' password.

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

Now, in order to make our `App` capable of authenticating our users, we have defined
`YesodAuth` instance for our `App` and its requirements.
`AuthId App` type above was specifically defined to differentiate the session
of the access to our web by reading `UsersId` in the cookies (or something like that).
`loginDest` and `logoutDest` handle what should a user see after he logged
in and logged out to/from his session.
while `redirectToReferer` determines whether should a user be redirected
to the route before he visit `loginDest` or not.

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
Although the order is fucked up just fill it casually.
(for further read, please read Mr. Snoyman's post about conduit and console input
which I can't remember where is it.)
When you have run it, look at the database (`select * from users`) and you will
see that our data has been inserted there.

Progress til now: [this](https://gitlab.com/ibnuda/Cirkeltrek/commit/8525a8c28475025afbb6566a92df04a6bd2ad61f)

### Authentication, Login & Logout

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
2. We put this line in order to get the "auth status" of the incoming requests.
3. We used the point 2's result to determine whether we should display login link
   or logout link.
4. This is where the app guards the routes.
   We specifically defined that only requests that satisfy `isLoggedIn` be able to
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

### Default Layout

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
the template ([hamlet, julius, and cassius](https://www.yesodweb.com/book/shakespearean-templates)) files.
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

I will hereafter always skip hamlet contents for the sake of cutting the word counts
down.
You can still see the commited files at the of sub-sub-sub-section as usual though.

And now, when load our site in the browser and see its source, you will see the
structure of our templates.

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
And surely it relates to GHC and Template Haskell restrictions.

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
And then reload our site and you'll see that our css is applied to the
layout.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/f21104e9cabd3c52f10c9c0b37bd906b582e9829).

#### Login Layout

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

### Completion of Foundation

Now that we have the needed foundation that provides:

- Basic routes, which has protected content, authentication, and public content.
- Layouts.

The next part is where we clone (most of) the functionalities of FluxBB.

[commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/143696db00a6781cc4081c335c8069b2b5f197d2).


## Cloning Things

Here, we will try to copy the functionality of FluxBB.
Although it won't 100% parity

### Preparations and Convention

Before doing anything, we should create a convention about what the handlers do.
For me, personally, what handlers should only do are:

- Serve a route for the rightful requests.
- Determines what should they do with the data they receive.
- Be as dumb as possible.

And what a handler should not do:

- Talk to the database.
- Implement business logic in it.

My weird code styles:

- DataTypesInCamelCase
- functionsInCamelCase
- valuesinlowercase
- 2 space indentation. Deal with it, mi amigo! 😎

Now, let's start by cleaning up our imports.
For example, instead of importing `Yesod.Core`, `ClassyPrelude.Yesod`, etc in
each and every file, we can just import a single file that has exported other modules.
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
What we just did was to simplify our imports by "combining" our import modules into
a single module.

And create a directory named `Handler` to contain every handler we have.
Plus, don't forget to update the name of the module that responsible for handling
requests (`Home` and `Profile`).
The purpose of this move is to ensure that we separate our (modules') concerns.
Other than that, I dislike a disastrous `~/` or any root project.

Now, we have cleaned our root project directory, let's start.

### Category Administration

If you're wondering why do we start with administering category, then I will
confess that I feel I can solve this problem (writing this article) better when
I approach it top-down.
It doesn't mean that you should the same, but, I don't know.
Just like Sinatra has sung, I will do it my way~

Okay, administering category in this context actually just means that we can create
categories and delete them, if the situation demands us to do so.

Based on the requirements above, we should:

- Ensure that only `Administrator` that could create and/or delete categories.
- Ensure that the forums of a category should be also deleted when we delete its
  parent category.

Therefore, we should create functions that satisfy the requirements above.

#### Category Creation: Business Logic and Database Query

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

#### Category Creation: Route and UI

In this page, a FluxBB administrator would see a page filled with something like the following
(which was dumbed down a lot):
```
├── New Category
│     Category Name [ input ]
│       [button create]
│
├── Category List
│     Categories [ dropdown ]
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

Data forms above are just our usual yesod's forms.
Nothing special there.
But when you see `selectCategoryForm`, you will see that it takes a list of `Entity Category`
We will use that to create a dropdown list that contains `Groups`' names and `Groups`' ids.
And then we will use `createCategoryForm` to generate a post-form which also be defined later.

The next step is modifying our `Foundation` to support the route and miscellanea.

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
        liftHandler $ runDB $
          select $ from $ \(group, user) -> do
            where_
              (user ^. UsersId ==. val uid &&. group ^. GroupsId ==. user ^. UsersGroupId)
            limit 1
            return (group ^. GroupsGrouping)
      return $ Just (uid, usersUsername user, unValue gro)

```
The function above retrieves userid, username, and group from the following columns from database:
`users.id`, `users.username`, and `groups.grouping` by doing the following things:

1. Gets the authentication status of a request.
2. In case of the absence of authenticated status, it returns nothing.
3. Otherwise, it queries the database what is the `Grouping` of a user
   with id `uid`.
4. Returns the group and and wrap them into a triple.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/be2dd0d1f592a80cf11c36b487d109125038eeb6)

And I'm sorry for commiting so often.
I'm afraid of commiting mistakes when writing this article.
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
---------------|-----------|---------------------------------------------------------------------------------
 Administrator | iaji      | sha256|17|xxxxxxxxxxxxxxxxx1f9aQ==|2WD50Li/BMLN/nOtw9C7PALXzK4YSPgAcfnRPpRYgfU=
 Moderator     | moderator | sha256|17|xxxxxxxxxxxxxxxxx1f9aQ==|2WD50Li/BMLN/nOtw9C7PALXzK4YSPgAcfnRPpRYgfU=

```

Login with that new user, open this route, and you will be greeted by "You're not the admin of this site."
Isn't it nice? It's nice.

The next part is where we actually creating the new category.
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
away.
Then the `res` is the result of post-form parsing by `runPostForm` using `createCategoryForm`
as the "template" of input.
In case of form failure or anything that is not a succesful result, we will let them be.
Other than that, we will create the group and then redirect back to this route.

Now, try it and look the database. You will see something like the following.
```
cirkeltrek=# select * from categories;
 id |   name
----|-----------
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
At the first function, I just want to show you a nice DSL.
At the second function, we just "lift" the execution of the first function (which
actually is a query).
At the third function, which we just modified it a little, we added a dropdown for
the categories which generated from the categories in the database.
In short, just check it out at your browser, my dude.

There's a bug, though. When you click "delete" what the system does is it inserts the key of
the shown category to as a new category.
We will fix that in the next section.

Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/a42e7196598b778169c984d966a6720290897687).

#### Category Deletion: Business Logic and Database Query

I'm pretty sure you're familiar with HTTP Verbs and RESTful API (lol, "API").
When we want to create something, we would use `POST`.
When we want to modify something, we would use `PATCH` or `PUT`.
When we want to delete something, we would use `POST`.
When we want to have a giggle, we would use `GET`.
Unfortunately, in browser, there's no support for this kind of neat interface.
Apparently, nobody has the time to write the specifications for verbs other than 
``GET` and `POST`.
I don't know why, my friend.

So, why did I write the previous paragraph? Because it relates to the undesired
behaviour at the previous section.
In an ideal world, we can just define the method for the forms and create the
corresponding yesod handler.
Unfortunately, there's no such thing as `ideal` in this world.
So, we will try to fit our program in.

Basically, `Form`s request formats in yesod are determined by, at least, two things.

- `Form`'s field itself.
- `method` and `submit` input.

It's better to show the code.
```
-- src/Handler/Adm/Category.hs
getAdmCategoryR :: Handler Html
-- skip
  defaultLayout [whamlet|
    -- skip
    <form method=post enctype=#{enctc}>
      ^{widc}
      <input .button-primary name=create value=create type=submit>
    <hr>
  |]

```
Please see the name tag of the `input` element above.
It has `create` as its name and `create` as its value.
When you press the "Create" button at the previous section, the format of the request will look like something
```
curl -d "_token=JdwQprBqre&f1=Ayyy+lmoa&create=create" -H "Content-Type: application/x-www-form-urlencoded" \
  -X POST http://localhost:3000/admin/category

```
Basically, the data sent by yesod-generated form in a request could be added by our own
defined input.
And using that kind of method, we can shoe horn our route handlers to handle deletion and creation.

Okay, let's modify our `postAdmCategoryR` into something like the following.
```
  (uid, name, group) <- allowedToAdmin
  createparam <- lookupPostParam "create" -- [1]
  deleteparam <- lookupPostParam "delete" -- [2]
  case (createparam, deleteparam) of -- [3]
    (Nothing, Nothing) -> invalidArgs ["At least be sure of what you want."] -- [4]
    (Just _, Nothing) -> do -- [5]
      ((res, _), _) <- runFormPost createCategoryForm
      case res of
        FormFailure x -> invalidArgs x
        FormSuccess r -> do
          _ <- createCategory group (createCategoryFormName r)
          redirect AdmCategoryR
        _ -> invalidArgs ["Good job, smarty pants!"]
    (Nothing, Just _) -> do -- [6]
      allcategories <- getAllCategories
      ((res, _), _) <- runFormPost $ selectCategoryForm allcategories
      case res of
        FormFailure x -> invalidArgs x
        FormSuccess r -> do
          defaultLayout [whamlet|
            I see, you want to delete #{selectCategoryFormId r}
          |]
        _ -> invalidArgs ["Good job, smarty pants!"]
    (Just _, Just _) -> invalidArgs ["Make up your mind, my dear admin."] -- [7]

```
There are a few modifications that we use here.

1. We look `create` from the post parameters.
   It could be `Nothing` when user clicked `delete` button.
2. Same as the first point.
   But this time we deal with `delete`.
3. Our standard pattern matching because both of `createparam` and `deleteparam` are `Maybe Text`
4. When there's no `create` nor `delete`, we can't do nothing.
   And `invalidArgs` you go!
5. When the user decided to press `create` button, we will continue as usual (before
   creation `delete`).
6. When the user decided to press `delete` button, we will try to delete the category
   with `categories.id` at the selected dropdown.
   We will create this later.
7. When the user decided to press both of them, which is impossible, or just handcrafted
   the request to include both of the value, we will just throw `invalidArgs`.

Now, we will continue point 6 above.
In order to delete we have to have deleteion query and guard thing at the business
logic.

To the `src/DBOp/CRUDCategory.hs` we go!
```
deleteCategory cid = do
  cat <-
    select $
    from $ \category -> do
      where_ (category ^. CategoriesId ==. val cid)
      return category
  forM_ cat (deleteCascade . entityKey)

```
If you're an astute reader, you will question why did we use `mkDeleteCascade` at
`src/Model.hs`.
Here's why we use delete cascade.
Remember, because `Forums` has a field that references `CategoriesId`, we can't
just delete it plainly.
We have to be ruthless by using `cascade` so everything that related to `Categories`
will also be deleted.

Now, the business logic, `src/Flux/AdmCategory.hs`,
```
deleteCategoryCascade Administrator cid = liftHandler $ runDB $ deleteCategory cid
deleteCategoryCascade _ _ =
  permissionDenied "You're not allowed to do this (category deletion)."

```
It's stupidly simple.
Only `Administrator` could delete it.
Surely, we have filtered any other user `groups` at the first line of the `postAdmCategoryR`,
but let's be defensive.
Trust nobody, not even yourself.

Two previous function additions now followed by `postAdmCategoryR` modification,
```
(Nothing, Just _) -> do
  allcategories <- getAllCategories
  ((res, _), _) <- runFormPost $ selectCategoryForm allcategories
  case res of
    FormFailure x -> invalidArgs x
    FormSuccess r -> do
      deleteCategoryCascade group $ toSqlKey $ selectCategoryFormId r
      redirect AdmCategoryR
    _ -> invalidArgs ["Good job, smarty pants!"]

```
It's pretty simple, you see.

- We get the categories.
- Use it to compare the values for `selectCategoryForm` check the validity (I guess).
- If the result of the form parsing is a OK, we just delete it.
- Other than that, well... Just throw `invalidArgs`.

#### Category Deletion: Route

Now, let's modify `src/Foundation.hs` to include `POST` method on `/admin/category`.
```
mkYesodData
  "App"
  [parseRoutes|
    -- skip
    /admin/category  AdmCategoryR GET POST
  |]

```
That's it.
It wraps up this section.

Commit: [this one](https://gitlab.com/ibnuda/Cirkeltrek/commit/de5e9c343b54c99f48fc7d41e8198b3794d2a211)!

### Forum Administration

This section's requirements are pretty much the same as the previous one.

- Ensure only an `Administrator` that could create and/or delete forums.
- Ensure that when the forum get deleted, its topics and posts should also be deleted.

#### Forum Creation: Business Logic and Query
Better look at `src/Model.hs` when we forget the structure of the datatypes, right?
There, we have `forums.category_id`, `forums.name`, `forums.descriptions`, and 
some fields which have default values.

Let's create a file in `src/DBOp` and give it `CRUDForum.hs`
```
insertForum cid name desc = insert_ $ Forum cid name desc 0 0 Nothing Nothing Nothing

```
You see that underscore (`_`)? That means we don't care the result.
We actually should just do the same on the previous section because the result
will be shown directly when the operation turns out to be succesful.

We will also use the same logic with the previous section, which guards the creation
to only user with `Administrator` group.
```
createForum Administrator cid name desc =
  liftHandler $ runDB $ insertForum cid name desc
createForum _ _ _ _ =
  permissionDenied "You're not allowed to do this (create forum)."

```

#### Forum Administration: Route and UI

On this page users (`Administrator`, to be exact) expect that they will see
```
├── New Forum
│     Forum Name [ input ]
│     Forum Desc [ input ]
│     Category   [ dropdown ]
│       [button create]
│
├── Forums
│     Category Name
│       Forum Name [ button delete ] [ button edit ]
│       ....
│       Forum Name [ button delete ] [ button edit ]
|     ....
└──

```
Let me be honest here because I should have said it at the opening section,
editing forum will create a cumbersome thing to do.
In FluxBB, other than editing the existing values, you will see permissions
and redirect url.
These two feature, will increase the complexity of this project by 3 knots.
Yes, I'm pulling that number from my ass, true.
And it's also true that I'm too lazy to implement it system-wide.

First, we will create a form that satisfy `New Forum`.
```
data CreateForumForm = CreateForumForm
  { createForumFormName :: Text
  , createForumFormDesc :: Maybe Textarea
  , createForumFormCategory :: Int64
  } deriving (Show)

createForumForm :: [Entity Categories] -> Form CreateForumForm
createForumForm cats = renderDivs $
  CreateForumForm
  <$> areq textField "Forum Name" Nothing
  <*> aopt textareaField "Description" Nothing
  <*> areq (selectFieldList catlist) "Category" Nothing
  where
    catlist =
      map (\(Entity cid (Categories name)) -> (name, fromSqlKey cid)) cats

```
`data CreateForumForm` above has the same explanation as the previous section.
But this time, there is a new field with `Maybe Textarea` and an `Int64` for `CategoryId`.

And don't forget to add a route entry at `src/Foundation.hs` and `src/Application.hs`.
```
-- src/Foundation.hs
mkYesodData -- skip
    /admin/forum     AdmForumR    GET

-- src/Application.hs
import Handler.Adm.Forum

```
Now, let's add a `GET` handler for the route above.
```
getAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  (wid, enct) <- generateFormPost $ createForumForm allcategories
  defaultLayout $ do
    [whamlet|
      <h3> Create Forum
      <form method=post action=@{AdmForumR} enctype=#{enct}>
        ^{wid}
        <input .button-primary name=create value=create type=submit>
    |]

```
You will see that we're using the same request guard (or something like that) with
`allowedToAdmin` in `Handler.Adm.Category`.
It's better to move them to a single place.
And I heard that `Foundation` is the right place for it.

Now, let's create the handler for the previous snippet.
```
postAdmForumR = do
  (u, n, g) <- allowedToAdmin -- [1]
  allcategories <- getAllCategories -- [2]
  ((res, _), _) <- runFormPost $ createForumForm allcategories -- [3]
  case res of
    FormFailure x -> invalidArgs x -- [4]
    FormSuccess r -> do -- [5]
      createForum
        g
        (toSqlKey $ createForumFormCategory r)
        (createForumFormName r)
        (unTextarea <$> createForumFormDesc r)
      redirect AdmForumR
    _ -> invalidArgs ["Good job, smarty pants!"] -- [4]

```

Basically the function above does:

1. No admin, get out!
2. Gets all categories.
   Please have a look at `src/Flux/AdmCategory.hs`.
3. Form parsing using "seed" from the previous point.
4. Standard "get out."
5. The real meat.
   We just extract the fields of the result of form parsing above, save it to the
   database, and then redirect to `AdmForumR`.

And that's it.
I really suggest you to open `approot/admin/forum`, input your values, and then
have a look at the database.
Here's mine.
```
cirkeltrek=# select * from forums;
 id | category_id |   name   | descriptions | topics_count | replies_count | last_post | last_post_id | last_poster
----|-------------|----------|--------------|--------------|---------------|-----------|--------------|-------------
  1 |           1 | Frist!!! | Nice, homie! |            0 |             0 |           |              |
(1 row)

```
Current progress: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/1a62efe47844582a5ba047827d71e6466aa8a773).

Wait!
We haven't shown our forum listings!
In FluxBB, the layout is something like the following:
```
├── Forums
│     Category Name
│       Forum Name [ button delete ] [ button edit ]
│       ....
│       Forum Name [ button delete ] [ button edit ]
│     ....
└──

```
Now, we have to simply query the forums, grouped by their categories.
Okay, let's create the query.
```
selectAllForumsAndCategoryName :: ReaderT backen m [(Text, Maybe [Text], Maybe [Key Forums])]
selectAllForumsAndCategoryName = do
  catnameandforums <-
    select $
    from $ \(category, forum) -> do
      where_ (category ^. CategoriesId ==. forum ^. ForumsCategoryId)
      groupBy (category ^. CategoriesName)
      return
        ( category ^. CategoriesName
        , arrayAgg (forum ^. ForumsName)
        , arrayAgg (forum ^. ForumsId))
  return $
    map (\(a, b, c) -> (unValue a, unValue b, unValue c)) catnameandforums

```
It's a simple query at `CRUDForum` which basically tell get forums and its category name.
Nothing interesting, actually.
Apart from `arrayAgg`, which aggregates the result.

And for the handlers, we will use standard `liftHandler`.
```
getForumsAndItsCategory = do
  something <- liftHandler $ runDB $ selectAllForumsAndCategoryName
  return $ map (\(cname, mts, mks) -> (cname, spread mts mks)) something
  where
    spread (Just ts) (Just ks) = zip ts ks
    spread (Just _) Nothing    = []
    spread Nothing _           = []

```
Considering the result of the previous function could contains `Nothing`,
we should clear 'em a bit using `spread` function above.
Though the function above could be cleaned a little by skipping `Nothing`.

And then use it on the handlers.
```
  defaultLayout $ do
    [whamlet|
     -- skip
      <form method=post action=@{AdmForumR} enctype=#{enct}>
      $forall (catname, fnamekeys) <- catfnamekeys
        <h4> Category: #{catname}
        <table>
          <thead>
            <th width="70%"> Name
            <th> Delete
          <tbody>
            $forall (name, key) <- fnamekeys
              <tr>
                <td> #{name}
                <td> <input name=delete-forum-id value=#{fromSqlKey key} type=checkbox>
      <input .button-primary name=delete value=delete type=submit>
    |]

```
The snippet above is a part from the `getAdmForumR`.
I think that snippet is pretty clear. Though it takes too much of space.
From this commit section on, we will put them in the `templates` directory.

For the deletion query, we will use the same function that looks **really** similar with
the category deletion above.

```
deleteForumById fid = do
  forums <- select $ from $ \forum -> do
    where_ (forum ^. ForumsId ==. val fid)
    return forum
  forM_ forums (deleteCascade . entityKey)

```
Again, just plain boring query. 

I swear, this part of logic is pretty boring.
Not even `forceTextToInt64` can save it.
Even the reason why do we use that is also boring.
Remember `lookupPostParam`? Its list equivalent returns
a list of `Text`, while what we need is a list of `Int64`
Surely we can throw `stupid user try again` or something like that.
But, let's keep them wondering why their malformed form never work
as "intended".

```
deleteForums Administrator fids =
  liftHandler $
  runDB $ forM_ fids (deleteForumById . toSqlKey . forceTextToInt64)
deleteForums _ _ = permissionDenied "You're not allowed to do this (delete forum)"

forceTextToInt64 :: Text -> Int64
forceTextToInt64 t =
  case readMay t of
    Just i  -> i :: Int64
    Nothing -> 0

```

This is the handler for `POST` request.
Using the same ugly technique as the previous section,
we will manage to determine what should be done. 
```
postAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  createparam <- lookupPostParam "create" -- [1]
  deleteparam <- lookupPostParam "delete" -- [1]
  case (createparam, deleteparam) of
    (Nothing, Nothing) -> invalidArgs ["What do you want? Create or delete?"] -- [1]
    (Just _, Just _) -> invalidArgs ["What do you want? Create or delete?"] -- [1]
    (Just _, Nothing) -> do -- [2]
      ((res, _), _) <- runFormPost $ createForumForm allcategories
      case res of
        FormFailure x -> invalidArgs x
        FormSuccess r -> do
          createForum g
            (toSqlKey $ createForumFormCategory r)
            (createForumFormName r)
            (unTextarea <$> createForumFormDesc r)
          redirect AdmForumR
        _ -> invalidArgs ["Good job, smarty pants!"]
    (Nothing, Just _) -> do
      deletions <- lookupPostParams "delete-forum-id" -- [3]
      deleteForums g deletions [4]
      redirect AdmForumR

```

Again, boring stuff.
Which is what a boring development like this should be.

1. The same reasoning and explanation with the "Category Management" above.
2. Our previous function.
   Which is used to create a category.
3. This is where we get the list of post parameter which I've talked about above.
   And it takes a `Text` and returns a `[Text]` and makes us to use `forceTextToInt64`.
4. Query execution.

Commit: [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/965ae3f0f0e38cc0b2e1241a835097d615fe1ca8).

That snippet wraps up this section.
Next, we should try to get display the Index of the page.

### Home, Categories, Forums, and Topics
Here we are, we are going to build this project by building it top down.
First, we will create the "home" page, where we display the list of the categories
and their forums.
After that, we will create the "forum" page, where we display the list of the topics
in a forum.
Followed by the the creation of the "topic" page which displays its replies.

Oh, don't forget that from now on, I will skip the snippet from whamlet.
It's too wordy.

#### Home (or Index)

Usually, when you visit FluxBB site's index, you will see that there are a few categories
and its forums shown.
To put simply, FluxBB's index structure is something like this
```
├── CategoryA
│  ├── [forumA name] [forum desc] [forum topics count] [forum replies count] [etc]
│  ├── ......
│  ├── [forumB name] [forum desc] [forum topics count] [forum replies count] [etc]
├── CategoryB
│  ├── [forumC name] [forum desc] [forum topics count] [forum replies count] [etc]
│  ├── ......
│  ├── [forumD name] [forum desc] [forum topics count] [forum replies count] [etc]

```
It's a tree!
And I want to emulate that.
In order to do that, we have should prepare a few things, like the query, logic, and "view" parts.

Let's start with the query part (which is ugly, to be honest).
```
-- src/DBOp/CRUDCategory.hs
selectCategoriesForIndex = do
  catnameandforums <-
    select $
    from $ \(category, forum) -> do
      where_ (category ^. CategoriesId ==. forum ^. ForumsCategoryId)
      groupBy (category ^. CategoriesName)
      return
        ( category ^. CategoriesName
        , arrayAgg (forum ^. ForumsId)
        , arrayAgg (forum ^. ForumsName)
        , arrayAgg (forum ^. ForumsDescriptions)
        , arrayAgg (forum ^. ForumsTopicsCount)
        , arrayAgg (forum ^. ForumsRepliesCount)
        , arrayAgg (forum ^. ForumsLastPost)
        , arrayAgg (forum ^. ForumsLastPostId)
        , arrayAgg (forum ^. ForumsLastPoster))
  return $
    map
      (\(a, b, c, d, e, f, g, h, i) ->
         ( unValue a
         , unValue b
         , unValue c
         , unValue d
         , unValue e
         , unValue f
         , unValue g
         , unValue h
         , unValue i))
      catnameandforums

```
Oh God... You see that multiple `arrayAgg`s and `unValue` parts?
Actually, what we want to `select` is simple, we just want to `select` categories and its forums.
The limitation is, we can't use `arrayAgg` for table, but only for columns.
It get worsen by the fact  I'm not familiar enough with Postgres to efficiently write a query that
returns multiple aggregated rows or something like that.
So, here we are, we have a function that is too wordy.
And that's not all, we have a more shittier version of it on our logic.

```
-- src/Flux/Home.hs
getCategoriesForIndex = do
  categoriesandforums <- liftHandler $ runDB $ selectCategoriesForIndex
  return $ map anu categoriesandforums
  where
    anu s =
      case s of
        (a, Just b, Just c, Just d, Just e, Just f, Just g, Just h, Just i) ->
          (a, zip8 b c d e f g h i)
        (a, _, _, _, _, _, _, _, _) -> (a, [])

```
You see that `zip8` function above?
There's no such thing in `base` because `base` only has `zip` thru `zip7`.
That means, we have to create that one by our hand.
Although it's not hard, as we can see the source of `zip7` in hackage, it's just
left a bad taste in my mouth.
And I'm starting to doubt my decision to use "tree" above.
But whatever, "if it's stupid and it works, it ain't stupid".

Now, let's head to the home handler.
Because it's pretty simple, we just have to put the previous function as one of
element and display it.
```
getHomeR = do
  categoriesforindex <- getCategoriesForIndex
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")

```
That's it, you can see that it actually is not worth mentioning.

Current progress: this [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/8faeb84586c57a406cf5fbd00b61715ec1817a2b).

#### Forums View

To put simply, here's what we usually see in a FluxBB Forum.
There are a few information that need to be shown.
For example, `Forum Name`, table (or is it a list?) of topic information, page, etc. 
```
 Index >> Forum Name
 Pages: [1] 2 3 .. n Next                                        [Post New Topic]
 ├── [Subject                  ][ Replies ] [  Views ][               Last Post ]
 ├── [This is a topic          ][    9001 ] [ 200010 ][ 2011-02-24 00:46 by tom ]
 ├── [This is not a topic      ][    6969 ] [ 100000 ][ 2011-02-24 00:42 by cat ]
 ├── [This is perhaps a topic  ][       0 ] [      3 ][                   Empty ]

```
From that information, at least we need to get data from database

- List of topics in that page.
  Which in turn each of them demands:

  - Title of topic.
  - Replies of topic.
  - Views.
  - And last post's information.
- The number of the pages.
- And "new post" link.

There are a few considerations, though.
For example, we won't implement pages for now because of I, embarrassingly,
forgot to put that.
Sorry.
We won't implement new post link either and will use a form at the bottom of
the page for the sake of simplicity (or my laziness?).

Well, it's better not to dwell in the past, right?

#### Forums Business Logic

Yes! We are coming to business logic part of this section!
This one is simple.
Or we are trying to dumb it down, I don't know.

- When there's a request to a forum with id `fid`, the program have to check
  whether it's associated to a certain user or not.

  - When it's not, program have to show the door to the request.
  - When it's associated to a user, program have to filter the group of the associated user.
  - When the associated user is a member of `Banned` group, the program will also show the door to the request.
  - Otherwise, we will show the the user the previous list at the previous section.
- When a user wants to create a topic by sending subject, program will

  - Create a topic with subject from user's input.
  - Insert the post with the post content from the user's input.
  - Using user's request's context, will complete the previous function's input.
  
#### Writing the Parts
Let's start by defining functions that satisfy the first point of the list above.

```
allowedToPost = do
  midnamegroup <- getUserAndGrouping
  case midnamegroup of
    Nothing -> permissionDenied "You're not allowed to see this page."
    (Just (uid, name, Banned)) -> permissionDenied "You're banned."
    (Just (uid, name, _)) -> return (uid, name, Administrator)

```
Practically, the function above mimics what `allowedToAdmin` does.
The only difference is, we swap `Administrator` with `Banned` and its
information.

Perhaps, when the situation arise, we can refactor functions that has the same behaviour
into a more "generic" function.
Or composable.
Or chainable.
Whatever.

After that, we will define the route for the forum and the forum page.
Don't forget that we identify forums by it's id, an `Int64`, so we can take
the parameter as `#Int64` in `mkYesodData`.

```
-- src/Foundation.hs
    -- snip
    /forum/#Int64        ForumR       GET POST
    /forum/#Int64/#Int64 ForumPageR   GET
    -- snip

```
You see that second `/forum`? That's for the page.
And don't worry about why did we put a `POST` at the higher line while not the bottom.

So, let's move forward to define `getForumR`, `postForumR`, and `getForumPageR`.
```
-- src/Handler/Forum.hs

getForumR :: Int64 -> Handler Html
getForumR fid = redirect $ ForumPageR fid 1 -- [1]

getForumPageR :: Int64 -> Int64 -> Handler Html
getForumPageR fid page = do
  forum <- _ (toSqlKey fid) -- [2]
  topics <- _ (toSqlKey fid ) page -- [3]
  (wid, enct) <- generateFormPost _ -- [4]
  defaultLayout $ do
    setTitle "Index"
    $(widgetFile "forum") -- [5]

```
There are a few marked points above.

1. Because when one load a forum, what one sees is the first page.
   Other than that, even when I'm a copy-paster, I hesitate to copy
   a function that not differ that much.
2. The first holed function.
   Based on the result of the left arrow, we can infer that this holed
   function is a `Handler` which returns a `Forums`.
3. The second holed function.
   But this time, we expect that this function returns `[Topics]`.
4. Remember what I have written above about "instead of inside of a link,
   we will put the form at the bottom of the page"?
   Yeah, this is what we're going to put at the bottom of the page.
5. Widget file.
   I won't say anything, just look at the commit.
   
Well, because the first and second holed functions are related to business logic,
we should head to the `src/Flux` directory and create a file named `Forum.hs` there.
(I know, I know... I can't be arsed to give them different names.)

```
-- first holed function.
getForumsInformation fid = do
  forum <- liftHandler $ runDB $ _ fid -- [1]
  case forum of
    [x] -> return x
    _   -> notFound

-- secod holed function.
getTopicsInForum fid page | page < 0 = invalidArgs ["Yo! You can't look at negative value!"]
getTopicsInForum fid page = liftHandler $ runDB $ _ fid page -- [2]

```
At the holed functions snippet above, there are two other holed functions, we'll talk
about it in a moment.
But first, we have to talk about the "first holed function"'s return value is an array
which is expected to have a single element.
Why would we want that function? Isn't there a function from `persistent` that
returns the first record it found (or not)?
Well, we're trying to have a consistent style here, my friend.
The previous database related functions use `select $ from $ etc` and I want
to continue the trend. 

Now, we will continue this section by writing the query for those holed functions above.
```
selectTopicsByForumIdPage fid page = do
  select $
    from $ \topic -> do
      where_ (topic ^. TopicsForumId ==. val fid)
      offset ((page - 1) * 25)
      limit 25
      return topic

selectForumById fid = do
  select $ from $ \forum -> do
    where_ (forum ^. ForumsId ==. val fid)
    limit 1
    return forum

```
Those two function should be placed at the holed place at the previous snippet. 
Both functions are just standard `select` procedure.
Nothing unusual here.
And furthermore, I will not put simple snippets anymore.
Other than "it's obvious", it also wastes space.

For next part, we will create a form for this hole `(wid, enct) <- generateFormPost _`.
Actually, we want to create a simple "subject" and "content",
pretty much like the simple `CreateCategoryForm` snippet above.

```
data CreateTopicForm = CreateTopicForm { createTopicFormSubject :: Text , createTopicFormContent :: Textarea}
createTopicForm = renderDivs $ CreateTopicForm <$> areq textField "New Subject" Nothing <*> areq textareaField "Opening Post" Nothing

```
That's it, Pretty simple.
It's just a text field and a text area.
And we will put them into their righteous place.
Oh, right, don't forget to update `src/Application.hs` to import `Handler.Forum`.
It will complain about `postForumR`, though.

So, we will create that handler.
```
postForumR :: Int64 -> Handler Html
postForumR fid = do
  (uid, name, group) <- allowedToPost -- [1]
  ((res, wid), enct) <- runFormPost createTopicForm -- [2]
  case res of -- [3]
    FormSuccess r -> do
      tid <- _ (toSqlKey fid) uid name (createTopicFormSubject r) (unTextarea $ createTopicFormContent r) -- [4]
      redirect $ ForumR fid -- we will back to it later.
    _ -> invalidArgs ["Come on..."]

```
Now, we have a handler for `POST` requests at `/forum/fid`
You see that number one? That's a nice function to filter the banned users from posting.
And number two and three? Just the standard procedure for parsing forms.
One thing, though.
We still have a holed function at number 4 which should be used to create a
topic which takes the required input from the context of the `POST` request.

```
createTopicByPosting fid userid username subject content = do
  now <- liftIO getCurrentTime
  tid <- liftHandler $ runDB $ _ fid username subject -- [1]
  pid <- liftHandler $ runDB $ _ tid 1 username userid content -- [2]
  return tid

```
Again, pretty simple function.
We will create a `Topics` first and then feed the return value as one of the input
for the `Posts` creation.
Then, we will return the `TopicsId` so the user can be redirected to the topic which
he has created in this process.
Again, I won't post the query snippets because it's too straight forward and too long.

Now, when we load [http://localhost:3000/forum/1], we will be greeted by a list of
created topics (which is zero, at the moment).
Go ahead and create that topic.

This wraps up this section.
For the next section, we will create handlers for administering forum.
Basically, locking and unlocking topics in a forum.
Current progress: this [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/e45b6107ac1ec3295f44589a911244f2e2e6604f)

#### Administering Forum

For a `Moderator` or an `Administrator`, from time to time, when the moon is blue enough,
he wants to lock an unworthy topic.
Or perhaps when he's wondering how could a top tier topic being locked, he will try to
make it right, unlock the good shit.

So, when an admin or a mod visits a forum, he will see the list of topic from and their
lock status.
When a topic is locked, he will see a column that says "it's locked, mate." at the right
side of a checkbox.
Well, it's not that important how we present the document, I think.
Better we go to handler etc.

```
postForumR fid = do
  (uid, name, group) <- allowedToPost
  lock <- lookupPostParam "lock-topic" -- [1]
  unlock <- lookupPostParam "unlock-topic" -- [1]
  create <- lookupPostParam "create-topic" -- [1]
  topicids <- lookupPostParams "topic-id" -- [2]
  case (lock, unlock, create) of
    (Just _, Nothing, Nothing) -> undefined [3]
    (Nothing, Just _, Nothing) -> undefined [3]
    (Nothing, Nothing, Just _) -> do -- [4]
      -- the previous section.
    _ -> invalidArgs ["🙄 (it's just an excuse to use an emoji.)"]

```
If you see that number `[1]`, it's just standard check `POST` parameter with parameter name
the quoted string and on the left side of the arrows are the results of the checking.
The results have `Maybe Text` as their datatype, considering that they may or may not
be used.
But number `[2]`, it's pretty much the same, but expects multiple occurences of the said
named parameter.
While we didn't modify number `[4]`, we just indent the previous code 2 or 3 tabs because it
does what it should do.
And surely, we are going to use this marked `[3]` to lock/unlock the topics, but before that,
let's write it first.

Actually, locking and unlocking topics is simple, we just swap the value of `topicsIsLocked`
and that's it.
Surely we have to make sure that we don't accidentally lock a topic in a row and only Admins
and Mods who can do it.

```
lockUnlockTopic lock group tid
  | group == Administrator || group == Moderator = do
    topic <- _ . toSqlKey . forceTextToInt64 $ tid
    if (topicsIsLocked $ entityVal topic) == lock
      then invalidArgs ["You can only switch the lock of the topic."]
      else liftHandler $
           runDB $ _ (toSqlKey . forceTextToInt64 $ tid) lock
lockUnlockTopic _ lock _ = permissionDenied "You're not allowed to lock this topic."

```
That's it.
We filter `group` parameter whenever this function being called.
Not only that, by comparing `topicsIsLocked` with `lock`, accidentally un/locking
topic twice shouldn't be happened.
But wait! There's a holed function which returns a topic while we have no such function!
No!!!

Okay, don't worry, we will create them in a file named `src/Flux/Topic.hs`.
```
getTopicById tid = do
  topics <- liftHandler $ runDB $ selectTopicById tid -- please see selectTopicById in the commit.
  case topics of
    [x] -> return x
    _   -> notFound

```
Nice and simple!
Just like our `getForumsInformation`!

Now, let's update our `postForumR` function to reflect the usage of `lockUnlockTopic`.
```
postForumR fid = do
  -- snip!!!
  topicids <- lookupPostParams "topic-id"
  case (lock, unlock, create) of
    (Just _, Nothing, Nothing) -> do
      forM_ topicids $ lockUnlockTopic True group -- [1]
      redirect $ ForumR fid
    (Nothing, Just _, Nothing) -> do
      forM_ topicids $ lockUnlockTopic False group -- [2]
      redirect $ ForumR fid

```
Well, the only difference between `[1]` and `[2]` is just the first param.
The `True` one, is being used to lock topic while the `False` one, to unlock.
Because `topicids` is a list of `Text`, we should iterate that value and
use that iterated value as the parameter for `lockUnlockTopic`.
And why do we use `forM_`? Because I miss my first year of uni days.
Other than that stupid reason, it was caused by the fact `lockUnlockTopic` is
a function that returns `HandlerFor site a`.
Surely we can use `forM`, but the result will be `[()]` and we don't want that.
We want `lockUnlockTopic` returns nothing.

So, go ahead! Try to lock and/or unlock things! 
And for the next part, we are going to play with Topics!

Checkpoint: [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/3dbd396572929ff516bb6b5fc3ad0b69d7965a0d).

#### Topics Business Logic

In this part, we are going to refine our rules above.
For example, we can't reply nor load a non-existent topic.
There are a few other little things and we will address them in an orderly manner (I'll try).

But first, we will update our routes first to include
```
-- src/Foundation.hs
mkYesodData
  -- skip
  /topic/#Int64        TopicR GET POST
  /topic/#Int64/#Int64 TopicPageR GET

```
so we can access that route and followed to create a file for those two new routes
at `src/Handler/Topic.hs`.
We defined two routes for the this part, and it makes me realise something at the
moment of this writing.
The developer of Yesod favors REST approach when using this framework, based on his
stackoverflow answer.
While I personally agree with his stance, I feel conflicted about a few things regarding
this matter.

1. This program's result is intended to be consumed using browsers and their supported methods.
   Which is only `POST` and `GET`.
2. About path pieces, I dislike url with parameters, to be quite honest.
   I prefer my urls structured `domain/fist/second/third/path` with no additional parameters.
3. In a web application (and especially REST "API") which supports those six methods
   (or "verbs"), the second point could be easily achieved.
   Not so much in a web browser, without additional POST parameters or even specialised
   routes which serve the purpose to accept unsupported methods by common browsers, my second point
   (or my taste) is basically a stupid thing.

Okay, that's enough for the ramble.

You see, because there are three defined methods from two routes,
we have to create their handlers arccordingly.
First, we have to create a handler for `TopicR GET`.
```
getTopicR tid = redirect $ TopicR tid 1

```
Why do we use redirection?
Because the logic for these function is basically the same.

- We check the existence of the topic.
- If there's no such thing, returns `not found`.
- Else, returns the 25 post on that page by paging the page.
- Display the page.

That's it.
And let's see `getTopicPageR`.
```
getTopicPageR tid page = do
  (uid, name, group) <- allowedToPost -- [1]
  posts <- _ (toSqlKey tid) page -- [2]
  topic <- getTopicById $ toSqlKey tid -- [3]
  forum <- getForumsInformation . topicsForumId . entityVal $ topic -- [4]
  (wid, enct) <- generateFormPost _ -- [5]_
  defaultLayout $(widgetFile "topic") -- [6]

```
`[1]` is just our standard guard.
Only the rightful users who could post their replies to topics in our forums.
And please wait, actually, `[2]` and `[3]` should be swapped.
While I did state that we should check the existence of the topic first,
I wrote the wrong line at this part commit.
Forgive me, my friend, for I am just a human bean.
`[3]` is a holed function where it returns the posts of that topic on a certain page.
While we're doing that, we will ask the forum's information to the database on `[4]`.
It's just a poorman's breadcrumb's, you know.
Surely we will address it later (or maybe much much later, or even won't ever) about
its pros and cons.
Then, we are going to create form's widget at `[5]`.

Okay, let's address number five first.
Because basically it's just a textarea, we just wrap it like the following.
```
data PostForm = PostForm { postFormContent :: Textarea } deriving (Show)
postForm :: Form PostForm
postForm = renderDivs $ PostForm <$> areq textareaField "Reply Discussion" Nothing

```
That's it.
Nothing unusual here, let's move along.

The following step is creating the holed function for `[2]` where we get the posts of
the topic.
Based on our previous decisions, we should create a file at `src/Flux/` with `Post.hs`
as its name.
```
getPostsInTopic tid page
  | page < 1 = invalidArgs ["Have you seen something page 0 before?"] -- [1]
  | otherwise = liftHandler $ runDB $ _ tid page -- [2]

```
You see the number `[1]`?
That's right.
No one should be able to see pages under 1.
But when the page is more than or equals to 1, we will serve them.
But there's a holed function!
We should create it first!
What should we get? Posts!
Where should we define how to get it? At `src/DBOp/Post.hs`!
What are the parameters? Topic id and and page!
Great!
It's just a simple query, my friend.
Don't worry, no choice for them.
We will include them in the commit.
And here we are.
We've finished this part.

Checkpoint: [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/01218ec664ae7d707dfa7bf72e996964897c07f0)

Amended by: [this](https://gitlab.com/ibnuda/Cirkeltrek/commit/9e6a5d5e555ed48cef6d35af1211b622dce43d60)

### Post, I Guess

I don't know if this part deserves a section of its own or not.
But whatever, let's start by defining what can we do on a post.

- Create it. We've covered it at `Forum` creation and `Topic` reply. 
- Edit it. But only by its creator and/or Admin/Mod. I hate this kind of feature, actually.
- Use it as an "anchor". You know, in the original FluxBB, we can click at the latest post,
  and then get redirected at the tail of the topic. Or wherever that post might be.
  
#### Editing a Post

As I've written above, only its poster and/or a Mod/Admin who could edit a post.
Then, when a user wants to edit his post, the program should

1. Get his post, which implies checking the post's existence.
2. If the post in question doesn't exist, the program should throw a 404 error.
3. Otherwise, this program will fetch the post and its related information.
4. Then serve it in a textarea.

Therefore, the handler could be written as the following. 
```
getPostEditR pid = do
  (uid, name, group) <- allowedToPost
  (Value fname, Value fid, Value tsub, (Entity pid' (Posts tid num name' uid' t content))) <- _ $ toSqlKey pid -- [1]
  (wid, enct) <- generateFormPost $ editPostForm content -- [2]
  defaultLayout $ do
    $(widgetFile "post")

```
`[1]` above is the holed function that should return `forumsName`, `forumsId`, `topicsSubject`, and `Entity Post`.
Why do we use that? It's related to breadcrumb, my friend.
You can see the structure of the breadcrumb's html at the commit below.
Anyway, `[2]` is just standard form generation.
But it takes `content` from the retrieved post above.
Well, you can't edit a post when you don't have the original content, amirite?

Okay, let's write that holed function.
```
getPostParentInformation pid = do
  postandparent <- liftHandler $ runDB $ _ pid -- [1]
  case postandparent of
    []  -> notFound
    x:_ -> return x

```
You see, the problem is the holed function `[1]`.
Basically we only have to take a row from SQL query and then return it.
Although the query itself is just a standard `SELECT` query, I'm feeling eager
to put it in the snippet below.
```
selectPostAndItsParentsInfo pid = do
  select $
    from $ \(post `InnerJoin` topic `InnerJoin` forum) -> do
      on (forum ^. ForumsId ==. topic ^. TopicsForumId)
      on (topic ^. TopicsId ==. post ^. PostsTopicId)
      where_ (post ^. PostsId ==. val pid)
      limit 1
      return
        ( forum ^. ForumsName
        , forum ^. ForumsId
        , topic ^. TopicsSubject
        , post)

```
You see, we are joining tables!
The query above could be translated to
```
select forums.name, forums.id,
       topics.subject, posts.id,
       posts.topic_id, posts.number,
       posts.username, posts.user_id,
       posts.time, posts.content
  from posts inner join topics
  on topics.id = posts.topic_id
    inner join forums
    on forums.id = topics.forum_id
    where posts.id = ?

```
I will just assume that you're much more capable of reading SQL query than my poor self.
Now, because we have completed the required parts from `[1]` holed function of `getPostEditR`
function.
So, let's `editPostForm` and its datatype.
```
data EditPostForm = EditPostForm { editPostFormContent :: Textarea } deriving (Show)
editPostForm :: Text -> Form EditPostForm
editPostForm content = renderDivs $ EditPostForm <$> areq textareaField "Post's Content" (Just content)

```
Standard form, as usual.
And that's it, we have completed this `GET` section for `/post/#pid/edit` route.

Now, `POST` handler  for `/post/#pid/edit`, basically the program just have to:

1. Again, get the post in question.
2. Use the retrieved post as the "seed" for the widget. lol, seed.
3. Check the result of form parsing.
4. Update the post.

So, let's make it into a reality!
```
postPostEditR pid = do
  (uid, name, group) <- allowedToPost
  post <- getPostById $ toSqlKey pid
  ((res, _), _) <- runFormPost . editPostForm . postsContent . entityVal $ post
  case res of
    FormSuccess c -> do
      _ -- [1]
        uid -- Key Users
        group -- Grouping
        (toSqlKey pid ) -- Key Posts
        (postsUserId $ entityVal post) -- Key Users
        (unTextarea $ editPostFormContent c) -- Text
      redirect $ PostR pid
    _ -> invalidArgs ["Come on..."]

```
Sure, there is a holed function there with a fuckton of parameters.
Well, it's actually to ease our things, man. 

```
editPostByUidGroupAndContent _ group pid _ content
  | group == Administrator || group == Moderator =
    liftHandler $ runDB $ _ pid content -- [1]
editPostByUidGroupAndContent _ Banned _ _ _ =
  permissionDenied "Bruh... You've been banned. Please..."
editPostByUidGroupAndContent uid _ pid uid' content
  | uid /= uid' = permissionDenied "You're not allowed to edit this post."
  | otherwise = liftHandler $ runDB $ _ pid content -- [1]

```
You see that snippet function above? Yeah, we "filter" the parameters to satisfy
our requirements.
For example, mods and admins can modify any post they want. (I hate it, though)
Banned users, although it should be impossible to reach this point, they should
be kept away from accessing database or something.
And then, when the first and fourth parameters is the same, which means that the editor
and the author of the post is the same dude, we will let them edit the post.
Yeah, let's ignore the holed function there.
It's just a standard update query which you can look at the commit below.

#### Post as "Anchor"

Yeah, I don't know how should I call it.
In FluxBB, we can just click a link at the "Last post" and then get redirected
to the corresponding post.
I guess the implementation is something like this.
```
getPostR pid = do
  (uid, name, group) <- allowedToPost
  (Entity _ (Posts tid num _ _ _ _)) <- getPostById $ toSqlKey pid
  let page = floor $ (toRational num - 1) / 25 + 1 :: Int64
  redirect $ TopicPageR (fromSqlKey tid) page :#: ("post-" <> show num)

```
We just get the post itself to get the post number and topic id of where that post belongs to.
Well, the final result is we just redirect the request to the corresponding topic on a certain page,
specifically.

I guess that's it. I can't think any other kind of action one could do on a post.

Checkpoint: [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/5cf33497bda57a441b4c7aa6daed375af0d9d8fc)

### Users Administration

In this section, we strive to create a thriving forum with a lot of users.
But we can't have users if we don't have any conventient means to let them in.
And when we have a handful of users, there will be a lot of shitty dudes on the
internet who deserve to be banned from our nice forum.
There are also nice dudes (not that "nice") who help other member who are in needs,
perhaps they can do much more if we give them better tools by making them Mod or even
Administrator.

#### Registration

Until now, we only use a single user, `Administrator` without any other user.
I'm pretty sure you don't want to talk to a wall, correct?
So let's start by creating a handler for this thing in `src/Profile.hs` (and rename
it to `User.hs`, it's more fitting, I guess).
```
getRegisterR :: Handler Html
getRegisterR = do
  _ -- [1]
  (wid, enct) <- generateFormPost _ -- [2]
  defaultLayout $(widgetFile "register") -- Please look at the commit at the end of this section.

```
First, we have to guard this part so nobody is currently loggedin can access this part of site.
Then, we just generate the form. Simple.

Let's head to `Foundation` and define a function that fits `[1]`
```
isNotLoggedIn = do
  maut <- maybeAuth
  case maut of
    Nothing -> return ()
    Just _  -> redirect $ HomeR

```
That's it.
If there's no session associated with the request, we can safely ignore the result.
But, if there's a session on his name, we will make them go Home`R`.

Now, for the second holed function of `getRegisterR`, we can just simply define it as
```
data RegisterForm = RegisterForm
  { registerFormUsername :: Text
  , registerFormPassword :: Text
  , registerFormEmail    :: Text
  } deriving (Show)

registerForm :: Form RegisterForm
registerForm =
  renderDivs $ RegisterForm
  <$> areq textField "Username" Nothing
  <*> areq passwordField "Password" Nothing
  <*> areq emailField "Email" Nothing

```
I know, I know, there's a section about password confirmation field on "Forms" chapter in yesod-book.
I'm just lazy to do that.
And now, we have a nice and simple registration form for our to-be-users.

Let's continue by writing its POST counterpart.
```
postRegisterR = do
  isNotLoggedIn
  ((res, wid), enct) <- runFormPost registerForm
  case res of
    FormSuccess r -> do
      _ <- _ (registerFormUsername r) (registerFormPassword r) (registerFormEmail r)
      --   ^ [1]
      redirect HomeR
    _ ->
      invalidArgs
        [ "Your input doesn't contribute sufficiently" , " for this capitalistic society. Think about it."]

```
It's even simpler, actually.
We just "extract" the value of the form, and then check the parsed result, then insert the data
to our database.

When this program has to process a registration, it should

1. Check the username and email, because we don't want to have a non-unique user, correct?
2. When the username and/or email already exist in the database, it should throw an error (or whatever HTTP code.)
3. Otherwise, get `Key` for `Grouping`. A new user should only be a normal `Member`.
4. It should hash the password
5. And then save it to the database.

That's pretty simple, correct?
Let's head to `src/Flux/Profile.hs` and rename it into `src/Flux/User.hs`.
It's a fitting name, you know.
```
registerUser username password email = do
  _ username email -- [1]
  now <- liftIO getCurrentTime
  gids <- liftHandler $ runDB $ _ Member -- [2]
  case gids of
    [] -> invalidArgs ["Call the Administrator"]
    x:_ -> do
      password' <- liftIO $ makePassword (encodeUtf8 password) 17
      liftHandler $ runDB $ _ (entityKey x) username (Just $ decodeUtf8 password') email now -- [3]

```
Just like the step number one, we should use `[1]` to check the "uniqueness" of the username and email.
And then `[2]` should find the `Key` for `Member`.
While `[3]` is the function that responsible for inserting the new user's data.

Okay, we should create a function for `[1]` in the same file.
```
unusedUser username email = do
  users <- liftHandler $ runDB $ selectUserByUsernameOrEmail username email
  case users of
    [] -> return ()
    _ -> invalidArgs ["Username and/or email has been used."]

```
You see, we just throw an invalid args error when `selectUserByUsernameOrEmail` returns non-empty
list.
The query itself is simple, actually.
Even, holed function `[2]` and `[3]` at `registerUser` are just simple queries too.
So I won't include it here.

Anyway, we have completed the handlers for user registration.
So, we just simply include the route to our route definitons at `Foundation` module.
And then replace `import Handler.Profile` with `import Handler.User`.

#### User's Page

After user's registration, we want to see his profile, right?
That's simple, we just create the route for it and its handler.
```
getUserR uid = do
  (ruid, name, group) <- allowedToPost
  (Entity uid' user ) <- getUserById $ toSqlKey uid
  defaultLayout $(widgetFile "profile")

```
That's it!

Current: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/0ee09cbce1fc61959c294eb1f93e2fff26f8dc04)

PS: I did fuck up, buddy. Since [this](https://gitlab.com/ibnuda/Cirkeltrek/commit/e45b6107ac1ec3295f44589a911244f2e2e6604f)
commit, it always returns `Administrator` as the third value of the returned triple.
And the latest commit fixed it.

#### Ban

You know, after we have a few registered users, we have a few users.
From time to time, a rude dude will appear and throw his tantrum.
We could talk him to senses or just slam that m.f. ban hammer.
If we're in a bad mood, we surely will need that hammer.
So, let's create it.

But firstly, I prefer to keep a bit like FluxBB when it comes to ban maintenance.
The ban administration page should show a form of username which will be used for
the next step of ban procedure.
It should also shows a table of the currently banned dudes.

Other than that, only `Administrator` and `Moderator` who should be capable of
banning people while they're immune of ban hammer themselves.
Not only that, an already `Banned` user could not be banned anymore.
The story's a bit different if their ban hammer has been lifted and they're a normal
`Member`.
Pretty simple, right?

What should we do? Of course, the first step is to create the route and put it in `src/Foundation.hs`
and then create the handler module which goes to `src/Handler/Adm/Ban.hs`.

```
getAdmBanR :: Handler Html
getAdmBanR = do
  (uid, name, gruop) <- _ -- [1]
  (wid, enct) <- generateFormPost _ -- [2]
  bans <- _ -- [3]
  let banneds = map (\(Value uid, b, Value ename) -> (uid, b, ename)) bans
  defaultLayout $(widgetFile "adm-ban")

```

Surely `[1]` is how we "filter" the request so only the rightful group could access
this part.
Oh yeah, I just simplified that part.
```
allowedToPost = do
  muidnamegroup <- getUserAndGrouping
  case muid of
    Nothing -> permissionDenied
    (Just (uid, name, Banned)) -> permissionDenied "You're banned"
    (Just (uid, name, group)) -> return (uid, name, group)

allowedToMod = do
  (uid, name, group) <- allowedToPost
  case group of
    x | x == Administrator || x == Moderator -> return (uid, name, group)
    otherwise -> permissionDenied "You are not allowed to moderate this page"

allowedToAdmin = do
  (uid, name, group) <- allowedToMod
  case group of
    Administrator -> return (uid, name, group)
    _ -> permissionDenied "You are not allowed to administer this page"

```
See that beauty?
No, I still see a repetitive pattern, if I may say.
But at least, those functions reuse themselves.
And yes `allowedToMod` could be plugged to `[1]` in `getAdmBanR`.

And the next part, for the `[2]`, let's skip that because it just a form with
a single text field.
But `[3]`, deserves a bit of attentions.
Although it only shows the banneds, it should only shows the rows which it's `still_in_effect`
value equals to `True`.
But that's it.
Other than that, this part doesn't have much weird things.

Now, I did said about procedure of banning user.
After we put the name of the victim of ban hammer, we should create another form for
the reasons and few other.

```
data BanUserOptionsForm = BanUserOptionsForm
  { banUserOptionsFormUsername :: Text
  , banUserOptionsFormIp       ::Maybe Text
  , banUserOptionsFormMessage  ::Maybe Textarea
  }

banUserOptionsForm username =
  renderDivs $ BanUserOptionsForm
  <$> areq textField "Username" (Just username)
  <*> aopt textField "IP" Nothing
  <*> aopt textareaField "Message" Nothing

```
You see that `username` parameter there?
We are going to use that from `[2]` form in `getAdmBanR`.
Surely, we should create a handler to put `BanUserOptionsForm`, right?
We will create that and make it to only receive `POST` request.

```
postAdmBanOptionsR = do
  (uid, name, group) <- allowedToMod
  ((res, _), _) <- runFormPost banUserForm
  case res of
    FormSuccess r -> do
      (wid, enct) <-
        generateFormPost . banUserOptionsForm . banUserFormUsername $ r
      defaultLayout $(widgetFile "adm-ban-options")
    _ -> invalidArgs ["Fill your input correctly."]

```
Nothing particularly interesting here.
But make sure to see `adm-ban-options.hamlet` in `templates` directory of
this project because the previous handler will throw a `POST` request to `AdmBanR`.
Obviously, we are going to (ab)use `POST` params here.
Not only `AdmBanR` will receive `POST` request from `AdmBanOptionsR` but also
from `AdmBanR` itself in case of lifting the ban hammer.

Let's create `POST` handler for `AdmBanR`.
```
postAdmBanR = do
  (uid, name, group) <- allowedToMod
  names <- lookupPostParams "username" -- [a]
  ban <- lookupPostParam "ban" -- [b]
  case (names, ban) of
    ([], Nothing) -> invalidArgs ["Make up your mind!"]
    ([], Just _) -> do
      ((res, _), _) <- runFormPost $ banUserOptionsForm "x" -- [1]
      case res of
        FormSuccess r -> do
          _ uid name group (banUserOptionsFormUsername r) (banUserOptionsFormIp r) (unTextarea <$> banUserOptionsFormMessage r)
       -- ^ [2]
          redirect $ AdmBanR
        _ -> invalidArgs ["Your input is not correct."]
    (xs, Just _) -> invalidArgs ["Make up your mind!"]
    (xs, Nothing) -> do
      _ uid name group -- [3]
      redirect AdmBanR

```
It's pretty hectic, I know.
That `[a]` marked function above was designed to receive a list of value with `name` parameter of `username`
while `[b]` marked function will decide what the program should do when it receives a `POST` request
with `ban` as one of its parameters.
Do you know why did we put an `"x"` at that `[1]` marked function?
Actually, we can put any string there because "it's just a default value".
So, as long as the input from `Administrator` or `Moderator` is valid (lol, they're never),
the program won't use `"x"`.
Then, `[2]` marked function, in where we should slam the m.f. ban hammer.
I decided to let this function to take 5 (FIVE!!!) parameters because we have a lot of things
going on here.
Don't worry we will talk about it in a minute.
For the `[3]` marked function, it is where we lift the ban hammer for those who appeal.

Okay, let's create the business process of banning user.
```
banUser execid execname execgroup username ip message = do
  gusername <- liftHandler $ runDB $ selectGroupByUsername username -- [1]
  case gusername of
    [] -> invalidArgs ["There's no user named " <> username] -- [2]
    x:_ -> do
      let (uid, gid, group) = (\(Value u, Value gi, Value g) -> (u, gi, g)) x
      case (_ execgroup group, execid == uid) of -- [3]
        (Right _, False) -> do
          liftHandler $ runDB $ do
            updateUserGroupingByUsername username Banned -- [4]
            insertBan username ip message execid -- [5]
        (Right _, True)  -> invalidArgs ["You cannot ban yourself."]
        (Left x, _)      -> invalidArgs [x]

```
Don't worry about query function `selectGroupByUsername` which was marked by `[1]` above.
It just gets `UsersId`, `GroupsId`, and its `Grouping` of the user in question.
Because the result of `[1]` is like any other standard esqueleto queries, it returns a list.
So, when the list is empty, surely we don't have a user who has name which equals to `username` 
parameter above and we shall throw an error about the non-existence of a user with that name.
And that's the reasoning of marker `[2]`

Okay, mark number `[3]` deserves a simple explanation and snippet.
```
banResult Administrator Administrator = Left "Cannot ban an Admin"
banResult Administrator Moderator     = Left "Cannot ban a Mod"
banResult Administrator Member        = Right ()
banResult Administrator Banned        = Left "Cannot ban an already banned user."
-- skipped the rest.

```
Of course we should not be able to throw ban hammer to admin and mods.
But when it comes to normal member, we should let them ban that poor user.
Though we can't make an already banned user get banned.

So, let's continue with `[4]` and `[5]`.
Truth to be told, they're just simple update and insert queries.
I'll spare your sight from those simple functions.

That's it and we're good to ban, assuming you've updated your `Foundation` and `Application` tho.

The next part is lifting the ban hammer.
Because `[b]` marked function at `postAdmBanR` returns a list, we shall use function
`forM_` in our function so it will look like this.
`forM_ names $ _ uid name group` but don't forget.
`name` and `group` came from `allowedToMod`.
So those are the name and group of the requester.

For business thingy of this part, I guess it's pretty much the same with `banUser` above.
```
unbanUser execid execname execgroup username = do
  gusernames <- liftHandler $ runDB $ selectGroupByUsername username -- [1]
  case gusernames of
    [] -> invalidArgs ["There's no user named " <> username] -- [2]
    x:_ -> do
      let (uid, gid, group) = (\(Value u, Value gi, Value g) -> (u, gi, g)) x -- [33
      case (_ execgroup group, execid == uid) of -- [4]
        (Right _, False) -> do
          liftHandler $ runDB $ do
            updateUserGroupingByUsername username Member -- [5]
            updateBan username Nothing Nothing execid False -- [6]
        (Right _, True)  -> invalidArgs ["You cannot unban yourself."]
        (Left x, _)      -> invalidArgs [x]

```
See that? We only changed 3 things!
Function marked by `[4]`, value of the second function `[5]`, and `False` for the function `[6]`!
Let's show the snippet for function which was marked by `[4]`
```
unbanResult Administrator Banned = Right ()
unbanResult Moderator Banned     = Right ()
unbanResult Administrator _      = Left "Cannot unban non-banned"
unbanResult Moderator _          = Left "Cannot unban non-banned"
unbanResult _ _                  = Left "You have no right to do so."

```
Much simpler!
Only banned that can be unbanned.
And the rest can cry at the corner.

And I guess we've finished this part.
Don't forget to check the commit below.
I've changed a few things.

Checkpoint: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/8986abd49dea3186afecd70faa280a71ef60a6ab)

I've sneakily modified the layouts in this [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/39711d9e8cba445680174be0d7b6da497825c4a5) and [this](https://gitlab.com/ibnuda/Cirkeltrek/commit/42344b7dd255c8f124bbacd5b99d6539b44e4847)

#### Promote and Ban

Now, let's talk about promotion and banning dudes.
You see, when we want to promote or demoting users, we should pick them first, correct?
And when you want to pick them, you will need some criterias and a place you submit
the criterias at, won't you?
Great! Now we're on the same page.
We should create the route, handler, and their miscellania first.

```
getAdmUserR :: Handler Html
getAdmUserR = do
  (uid, name, group) <- allowedToMod
  groups <- _ -- [1]
  (wid, enct) <- generateFormPost $ _ groups -- [2]
  let users = []
  adminLayout uid name group $ do
    setTitle "Manage Users"
    $(widgetFile "adm-user")

```
Of course we use `allowedToMod` guard in this route.
We don't want a random user banning people as they wish, do we?
Then, we shall get the existing groups using function marked by `[1]`.
No worries, it's just a standard select.
But when you ask me why do we need that?
Remember about before? Where we talk about picking certain users first?
Grouping is one of those criterias.
There could be a scenario where we want to demote a stupid mod or something like that,
correct?
And surely we will use `groups` as the "seed" for the form we should create.
Though the form itself is nothing interesting.
It just wraps :

- Grouping of the users
- Username
- Email

Yeah, that's it.
We won't be needing a lot of criterias like what FluxBB does.
Oh yeah, we are our new custom layout for administration hereon.

What's next? We need to create a place where we receive the criterias
and then pass it to another part to process it.
```
postAdmUserR :: Handler Html
postAdmUserR = do
  (uid, name, group) <- allowedToMod
  groups <- getAllGroups -- [1]
  ((res, wid), enct) <- runFormPost $ searchUserForm groups
  let (mgid, musername, memail) =
        case res of
          FormSuccess r ->
            ( toSqlKey <$> searchUserFormGroupId r
            , searchUserFormUsername r
            , searchUserFormEmail r)
          _ -> (Nothing, Nothing, Nothing)
  users <- _ mgid musername memail -- [2]
  adminLayout uid name group $ do
    setTitle "Users"
    $(widgetFile "adm-user")

```
If you ask why do we receive those search criterias at the same route / handler,
because I just feel like it.
Again, I'm conflicted with url cleanliness, rest interface thingy, and yesod's routes.
Okay, what we have here?
Function marked by `[1]` is the same marked function of `getAdmUserR`.
Nothing special.
And about form parsing (or something like that), it's just our normal parsing.
But the difference here is we won't care any invalid criterias and just return a triple of `Nothing`.
Sure, it's a bad thing considering "best practices".
Truly, the spirit is ready but the flesh is weak.

Anyway, function which was marked by `[2]` above, is where we process the picking process and then
display the result.
Pretty simple, correct?
So, it's obvious that we are going to create that function at `src/Flux/User.hs` (bad choice, I know).

```
getUsersByConditions mgid musername memail = do
  userandgroup <-
    liftHandler $ runDB $ _ mgid musername memail -- [1]
  return $ map (\(user, Value group) -> (user, group)) userandgroup

```
Yeah, standard query transformation of `ReaderT` to `Handler`.
But the nice thing is what I've marked by `[1]`.
Look at the following snippet from `src/DBOp/User.hs`!

```
selectUsersByConditions mgid musername memail = do
  select $
    from $ \(user, group) -> do
      where_
        (    qbuilder user UsersGroupId mgid
         &&. qbuilder user UsersUsername musername
         &&. qbuilder user UsersEmail memail
         &&. user ^. UsersGroupId ==. group ^. GroupsId)
      orderBy [asc (user ^. UsersUsername)]
      return (user, group ^. GroupsGrouping)
  where
    qbuilder _ _ Nothing = (val True)
    qbuilder user accessor (Just v) = (user ^. accessor ==. val v)

```
You see that `qbuilder` function?
I feel so proud when I came up with that kind of solution, to be quite honest.
Though actually I just substitute a `Nothing` data with a `True` and otherwise
use the value in `Just a` to get the comparison of `a` and the field.
Which in turn we can chain compare any `Maybe` input with `NOT NULL` collumn.

Now, we have all the needed parts for searching a specific group of users.
And here's the checkpoint search and display: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/d83b2a0d12e7cf5f93f276d9ebbddb36b142c352)

Now, we have something like this pic
![alt text](images/cirkeltrek-adm-user.png "Search result.")

Now we have should use those two button to demote or promote users.
It's a great choice to ban those dudes first.

In FluxBB, the step to mass ban is something like this.

1. Select the users.
2. Press that "Ban" button.
3. Write the messages why do we ban them.
4. Execute.
5. ???
6. Mass banned!

Okay, pretty simple, right?

Let's the form and its handler.
And yes, I assume you have decided the route by yourself.
No, I won't show the snippet.
Anyway, here's the handler.
```
postAdmUserPromoteR = do
  promote <- lookupPostParam "promote" -- [1]
  userids <- lookupPostParams "user-id" -- [1]
  case promote of
    Just "ban" -> do
      (uid, name, group) <- allowedToMod -- [2]
      (wid, enct) <-
        generateFormPost $ _ $ intercalate "," userids -- [3]
      adminLayout uid name group $ do
        setTitle "Promote"
        $(widgetFile "adm-user-promote")
    _ -> invalidArgs ["What do you want to do with the users?"] -- [4]

```
The lines marked by `[1]` above are just to get the params.
We've talked about them a few pages above.
And why do we use `allowedToMod` at `[2]`?
Because mods could also ban people, not just admins.
For the holed function marked by `[3]`, we are going to create a simple form
with a hidden field.
Don't worry about `[4]`, we're going to address it later.

```
data BanUsersOptionsForm = BanUsersOptionsForm { banUsersOptionsFormMessessage :: Maybe Text , banUsersOptionsFormIds :: Text}

banUsersOptionsForm :: Text -> Form BanUsersOptionsForm
banUsersOptionsForm userid = renderDivs $ BanUsersOptionsForm
  <$> aopt textField "Message" Nothing
  <*> areq hiddenField "" (Just userid) -- [1]

```
That `[1]` is where we hide our victims' id.
I mean, we are just passing their id for the next phase but we don't actually want to watch them.

For example, we have a request body like the following:
```
user-id=7&user-id=6&promote=ban

```
And this handler should only show us this markup:

![alt text](images/cirkeltrek-adm-user-message.png "Shown message.")

I hope you are getting what I mean.

Okay, the next part is where we actually do the banning.
In this case, we should create another handler to parse the form with "ban message"
and hidden "user-id".
```
postAdmUserPromoteExeR = do
  promote <- lookupPostParam "promote"
  case promote of
    Nothing -> invalidArgs ["No promote param."]
    Just "ban" -> do
      (uid, name, group) <- allowedToMod -- [1]
      ((res, wid), enct) <- runFormPost $ banUsersOptionsForm "" -- [2]
      case res of
        FormSuccess r -> do
          let (mes, userids) = ( banUsersOptionsFormMessessage r , map forceTextToInt64 $ splitOn "," $ banUsersOptionsFormIds r)
          forM_ userids $ \userid -> _ uid name group (toSqlKey userid) Nothing mes -- [3]
          redirect $ AdmUserR
    Just "change" -> do
      error "Change"
    _ -> invalidArgs ["Can't understand that."]

```
Again, only mod and admin who can ban users, so we put `allowedToMod` at that `[1]`.
And why do we use an empty string as default value at `[2]`?
Surely because we don't want randomly ban people in case of we fail to parse a request.
And then the main part of this snippet is the holed function which was marked by `[3]`.
That line iterates the victims' userids and then pass them for the next banning process.

Let's write that function.
```
banUserById execid execname execgroup userid ip message = do
  [user] <- liftHandler $ runDB $ selectUserById userid -- [1]
  banUser execid execname execgroup (usersUsername $ entityVal user) ip message

```
Yeah, I didn't forget that we already had written a similar function to ban them.
We just make sure that the passed user id does indeed exist on`[1]`.
Then, we just call `banUser` to finish the deed. 

Checkpoint ban: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/f75c4acc8b075b14ec4bc5ff76cc545c86350c0c)

After finishing the ban hammer part, we are going to create the promotion part.
At the same handler, we are going to add a few minor changes.
For example, at `postAdmUserPromoteR` we will add a few lines.
But before that, we should know that in FluxBB, only admin who can promote users.
But, even admin could only promote (or demote them) from normal member to moderator
vice versa.
Surely there will be a few differences in places, but that doesn't really matter, I guess.

Okay, let's modify `postAdmUserPromoteR`.
```
postAdmUserPromoteR = do
  -- snip
  case promote of
    Just "ban" -> do
      -- snip
    Just "change" -> do
      (uid, name, group) <- allowedToAdmin
      let ban = False
      mo <- _ Moderator -- [1]
      me <- _ Member -- [1]
      (wid, enct) <- generateFormPost $ promoteUserForm [mo, me] $ intercalate "," userids -- [2]
      adminLayout uid name group $ do
        setTitle "Promote"
        $(widgetFile "adm-user-promote")

```
We should get the appropriate group for promotion (or demotion), so we are placing
those `[1]` for it.
Actually, `[1]` is pretty simple.
It just select the group from database, nothing more and nothing less.
Also, there's a form for promotion which doesn't differ much from `BanUsersOptionsForm`
where we hide a few things from users.
Another difference is we supply that form with a list of `Groups` for the sake of
the promotion.

After we modified `postAdmUserPromoteR`, we should also modify `postAdmUserPromoteExeR`.
Remember that we have to make sure the process of promotion doesn't go awry.

```
postAdmUserPromoteExeR = do
  promote <- lookupPostParam "promote"
  case promot
    -- snip
    Just "promote" -> do
      (uid, name, group) <- allowedToAdmin -- [1]
      mo <- getGroup Moderator
      me <- getGroup Member
      -- ^ [2]
      ((res, _), _) <- runFormPost $ promoteUsersForm [mo, me] ""
      -- ^ [3]
      case res of
        FormSuccess r -> do
          let (gid, userids) = (promoteUsersFormGroupId r , map forceTextToInt64 $ splitOn "," $ promoteUsersFormUserIds r)
          -- ^ [4]
          forM_ userids $ \userid -> _ uid name group (toSqlKey userid) (toSqlKey gid)
          -- ^ [5]
          redirect AdmUserR
        _ -> invalidArgs ["Please..."]

```
We use `allowedToAdmin` at `[1]` just to make sure that only admin could promote users.
Same goes as `[2]`, we only make sure that only allowed `Groups` are appearing in this part.
I mean, we don't want user sending administrator's `id`, right?
FluxBB devs say that it posesses security risk or something like that.
And for `[3]`, we use an empty string for the default value for the second param (where user-ids
being placed at) because where do we know the value of the sent requests?
So it's better to leave them empty.
And when the user's requests comply our form, we will process that at `[4]`.
Nothing really interesting here.
The second tuple is the result of a forced string of comma-separated integers being tranformed
back to list of integers.
God. What a weird sentence!

The actual working part is the function which was marked by `[5]`.
It iterates `userids` and then pass them to the holed function at the right side of the right arrow.

Now, let's create a function that fill that hole.
```
-- src/Flux/Adm/User.hs
promoteUser execid execname execgroup targetid targetgroupid = do
  guardUser execid targetid -- [1]
  guardGroup execgroup -- [2]
  enttargetuser <- getUserById targetid -- [3]
  enttargetgroup <- getGroupById targetgroupid -- [3]
  let username = usersUsername $ entityVal enttargetuser -- [4]
      group = groupsGrouping $ entityVal enttargetgroup -- [4]
  liftHandler . runDB $ do
    updateUserGroupingByUsername username Member -- [5]
    updateBan username Nothing Nothing execid False -- [5]
    updateUserGroupingByUsername username group -- [6]
  where
    guardUser xid tid = -- [1]
      if xid /= tid
        then return ()
        else invalidArgs ["Cannot promote or demote yourself."]
    guardGroup Administrator = return () -- [2]
    guardGroup _ = invalidArgs ["Only admin allowed to promote."]

```
To be honest, the function at the snippet above deserves a few explanations based on
their marks.

1. We don't want an admin demote himself.
   If we let him do that, there could be a forum where there's no admin.
   Chaos will surely ensue.
2. Again, we should make sure that only a user in `Administrator` group
   allowed to modify user's group.
   See point 1.
3. I can assure you.
   Those function only get their respective entities.
   Nothing more, nothing less.
4. Here, we get the entity's field. or column. or name.
5. Remember when I talked about a few differences in promoting/demoting users?
   Here's the difference.
   In FluxBB, we couldn't promote / demote banned users.
   Here, we will let them being promoted.
6. It's where we actually change the group of the user.
   Whether the user's group being changed to `Banned`, `Member`, or even `Moderator`,
   we change it here.
   
And that's it.
It concludes this section. 

Commit promote: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/24c0c6cba75a738cd0f15f60b6ae315cd8d7da61)

#### Reports

Let's be honest, we are going to absolutely have conflicts when a lot of people
gather in a single place.
In this case, the problems will arise from the forum posts.
So, we should accomodate the needs for <del>complaints</del> reports.

Usually, the flow of the reporting post is like the following.

1. User sees a troubling post.
2. User click `report` link.
3. User fill the reports.
4. Admin/Mod load the report page.
5. Admin/Mod check the reported post.
6. Admin/Mod do whatever they want with the post in question.
7. Admin/Mod zap that report.

Okay, let's start by creating handler for reporting.
```
-- src/Handler/Post.hs
getPostReportR pid = do
  (uid, name, group) <- allowedToPost
  (Value fname, Value fid, Value tsub, (Entity pid' (Posts tid num name' uid' t content))) <- getPostParentInformation $ toSqlKey pid
  (wid, enct) <- generateFormPost reportPostForm
  defaultLayout $ do
    setTitle "Reports"
    $(widgetFile "post-report")

```
You see, it's just standard `GET` handler, `Posts` information retrieval, and
normal form rendering.
Even `reportPostForm` itself is just a rendered `Form` with a single textarea
input.

That `GET` handler will be followed by writing its `POST` handler equivalent.
```
postPostReportR pid = do
  (uid, name, group) <- allowedToPost
  (Value fname, Value fid, Value tsub, (Entity pid' (Posts tid num name' uid' t content))) <- getPostParentInformation $ toSqlKey pid
  ((res, wid), enct) <- runFormPost reportPostForm
  case res of
    FormSuccess r -> do
      _ pid' tid fid uid (unTextarea $ reportPostFormComplaint r) -- [1]
      redirect $ PostR pid
    _ -> invalidArgs ["Please report correctly."]

```
Still our usual `POST` handler.
The only difference here is just that we should insert the `Reports` into
database.
Let's write it.
```
createReport pid tid fid uid message = do
  now <- liftIO getCurrentTime
  liftHandler $ runDB $ _ pid tid fid uid now message Nothing Nothing -- [1]

```
Again and again, that `[1]` function, it's just a standard `insert` operation.
I will spare wasting your time by skipping writing that holed function in this article.

That's it! We have finished report feature.
Also, don't forget to update `topic.hamlet` to link to `ReportR`.

Commit report handler: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/cf121cae529eecd7d074338cc80cc630b7d21fc9)

Surely, we won't let admin and mods wondering where to access those precious
reports, will we?
Therefore, we should create the desired path to administer that reports.

In FluxBB, admins and mods are allowed to see the latest reports and the "zapped"
reports (whatever that means) at the Reports Management page.
Not only we can see what are the reports are, but also we can see who has
created them, etc.
You get the gist.

So, firstly we should create that handler.
```
-- src/Handler/Adm/Report.hs
getAdmReportR = do
  (uid, name, group) <- allowedToMod
  unzappedreports <- _ -- [1]
  zappedreports <- _ -- [2]
  adminLayout uid name group $ do
    setTitle "Reports Management"
    $(widgetFile "adm-report")

```
Of course we are using `allowedToMod` guard, for the ones who are allowed to see
the reports just mods and admins.
Then, we should create two functions which retrieve the needed reports.
```
-- src/Flux/Adm/Report.hs
getAllUnzappedReports = do
  reports <- liftHandler $ runDB $ _ -- [1]
  return $ map (\(x, Value a, Value b, Value c) -> (x, a, b, c)) reports

getAllZappedReports = do
  -- snip [2]

```
Those two functions above practically the same.
The only difference is the first one doesn't return the mods/admins who "zapped"
the reports.
Even their queries don't differ that much.

Let me show you one of them.
```
selectReportsZappedInfo = do
  select $ from $ \(report, user, topic, forum, user') -> do
    where_
      (report ^. ReportsReportedBy ==. user ^. UsersId
        &&. report ^. ReportsForumId ==. forum ^. ForumsId
        &&. report ^. ReportsTopicId ==. topic ^. TopicsId
        &&. report ^. ReportsZappedBy ==. just (user' ^. UsersId))
    orderBy [desc (report ^. ReportsZapped)]
    return
      (report , user ^. UsersUsername , forum ^. ForumsName , topic ^. TopicsSubject , user' ^. UsersUsername)

```
That's it.
We just select from `reports`, `users`, `topic`, `forums`, and `users` tables
to get the required data for `AdmReport` page.

Now, when you load the route to the `AdmReport`, you will see reports from users.
Oh, don't forget that the mods/admins are capable of editing the posts themselves.
So, after we can response to that reports, it left us with "how to mark them as 'zapped'".
Of course, we should start by creating `POST` handler for that.
```
postAdmReportR = do
  (uid, name, group) <- allowedToMod
  reportids <- lookupPostParams "report-id"
  _ uid reportids -- [1]
  redirect AdmReportR

```
Yeah, it's pretty simple, too fricking simple, actually.
Even the holed function which was marked by `[1]` also simple.
Here
```
readReports uid reportids = do
  liftHandler $ runDB $ do
    let x = map readMay reportids :: [Maybe Int64]
        y = map (toSqlKey <$>) x :: [(Maybe (Key Reports))]
    forM_ y $ \z -> do
      case z of
        Nothing -> return ()
        Just x -> updateReportZap uid x

```
Yeah, we just map `[Text]` into `[Maybe Int64]` and then map them into `[(Maybe (Key Reports))]`
And when the transformed report id returns `Nothing`, we will just
return a unit.
Yes, even `updateReportZap` only update `reports` table to make them "zapped."

And that's it! We finished this part!

Commit report admin: [commit](https://gitlab.com/ibnuda/Cirkeltrek/commit/24c0c6cba75a738cd0f15f60b6ae315cd8d7da61)

### User Information

I don't know, man.
Why would one want to know what is a dude what up to in the internet?
That sounds like what a stalker do, doesn't it?
But whatever, it's okay, "you do you" like the wise usually says.

#### Searching Users

Well, you know, from time to time, we surely want to know about user's information
who has ignited our passionate interests when we're doing our stuff in this
forum.
So, it's only normal to have a feature to search users, isn't it?

Let's start by writing the handler first.
```
-- src/Handler/User.hs
getUserListR = do
  (uid, name, group) <- allowedToPost -- [1]
  let users = [] :: [(Grouping, Entity Users)] -- [2]
  ad <- getGroup Administrator -- [3]
  mo <- getGroup Moderator -- [3]
  me <- getGroup Member -- [3]
  (wid, enct) <- generateFormPost $ _ [ad, mo, me] -- [4]
  defaultLayout $ do
    setTitle "User List"
    $(widgetFile "user-list")

```
Of course we are using our beloved guard, `allowedToPost`, here!
It "flatten" our function's structure!
No need `case` or anything else! Neat!
And why do we need that empty list at `[2]`?
Because we are going to use that in our template.
And because they're empty, we won't have to worry about needless queries.
Though we will use that at `[3]` for our search form because we're just
an incomplete immitation of FluxBB.
And for the holed function which was marked by `[4]`, that's the form we're
using.
And let's write that form.
```
data SearchUserForm = SearchUserForm
  { searchUserFormUsername  :: Maybe Text
  , searchUserFormGroup     :: Maybe Int64
  , searchUserFormSortBy    :: SortBy
  , searchUserFormAscending :: Bool
  }

```
Look, that's what FluxBB's search form look like.
We have a field of nullable username, nullable group (adm, mod, member),
sorting field, and ascending / descending options.
And we should write our own "sort categories"
The category (`SortBy`) in and of itself is just a simple
sum type like the following
```
data SortBy = Username | Registered | PostCount deriving (Eq, Enum, Bounded)

instance Show SortBy where
  show Username   = "Username"
  show Registered = "Registration Date"
  show PostCount  = "Post Count"

```
Pretty much a copy and paste from Yesod's cookbook with a minor difference
in `Show` derivation.
Nothing really interesting here, to be quite honest.

```
searchUserForm groups = renderDivs $
  SearchUserForm
  <$> aopt textField "Username" Nothing
  <*> aopt (selectFieldList glist) "Groups" Nothing
  <*> areq (selectFieldList slist) "Sort By" Nothing
  <*> areq (selectFieldList alist) "Sort Order" Nothing
  where
    glist =
      map (\x -> ( pack . show . groupsGrouping $ entityVal x , fromSqlKey . entityKey $ x)) groups :: [(Text, Int64)]
    -- ^ [1]
    slist = map (pack . show &&& id) [minBound .. maxBound] :: [(Text, SortBy)]
    -- ^ [2]
    alist = [("Ascending", True), ("Descending", False)] :: [(Text, Bool)]
    -- ^ [3]

```
Here's the same old form rendering function.
The function marked by `[1]` is the exact same function in promotion form far above.
I should remind you that number `[2]`, again, is the carbon copy of `selectFieldList`
example from Yesod book.
Number `[3]`? Sadly it's pretty clear.
We shows "Ascending" or "Descending" in a dropdown input.

After you put the previous function to the holed function at `getUserListR`,
go to `UserListR` and you will see the rendered layout.
Sure it's a waste of space compared to the original FluxBB.
And unfortunately, I'm illiterate of HTML and CSS.
So, please spare me your anger. ;-;

The next part, is where we actually search things.
First, as usual, we should create the handler.
```
postUserListR :: Handler Html
postUserListR = do
  (uid, name, group) <- allowedToPost
  ad <- getGroup Administrator
  mo <- getGroup Moderator
  me <- getGroup Member
  ((res, wid), enct) <- runFormPost $ searchUserForm [ad, mo, me]
  case res of
    FormSuccess r -> do
      let (username, groupid, orderby, ascending) = (searchUserFormUsername r, toSqlKey <$> searchUserFormGroup r, searchUserFormSortBy r, searchUserFormAscending r)
      users <- _ username groupid orderby ascending -- [1]
      defaultLayout $ do
        setTitle "User List"
        $(widgetFile "user-list")
    _ -> error ""

```
Other than the holed function `[1]`, there's nothing much to explain.
We get the group entities, parse the form data from the request, and then
"extract" their values, `[1]` do its part, and then show the result to the
requester (is requester a word?).

Truth to be told, the function `[1]` itself is not really complicated.
Apart from `unValue` a thing or two, it just runs the query.
And the query, which I spent 2 hours for, is really neat, I guess.
```
-- src/DBOp/CRUDUser.hs
selectUsersBySearchConditions :: ( PersistUniqueRead backend , PersistQueryRead backend , BackendCompatible SqlBackend backend , MonadIO m)
  => Maybe Text -> Maybe (Key Groups) -> SortBy -> Bool
  -> ReaderT backend m [(Value Grouping, Entity Users)]
selectUsersBySearchConditions username groupid orderby ascending = do
  select $
    from $ \(user, group) -> do
      where_ (qbuilder group GroupsId groupid &&. qbuilder user UsersUsername username &&. user ^. UsersGroupId ==. group ^. GroupsId)
      --      ^ [1]
      ordering ascending user orderby
      -- ^ [2]
      return (group ^. GroupsGrouping, user)

qbuilder :: (PersistField a, Esqueleto query expr backend, PersistEntity ent) => expr (Entity ent) -> EntityField ent a -> Maybe a -> expr (Value Bool)
qbuilder _ _ Nothing           = val True
qbuilder ent accessor (Just v) = ent ^. accessor ==. val v

ordering :: Esqueleto query expr backend => Bool -> expr (Entity Users) -> SortBy -> query ()
ordering b user Username   = orderBy [(chooseAscension b) (user ^. UsersUsername)]
ordering b user Registered = orderBy [(chooseAscension b) (user ^. UsersJoinTime)]
ordering b user PostCount  = orderBy [(chooseAscension b) (user ^. UsersRepliesPosted)]
--                                    ^ [3]

chooseAscension :: (Esqueleto query expr backend, PersistField a) => Bool -> expr (Value a) -> expr OrderBy
chooseAscension True e  = asc e
chooseAscension False e = desc e

```
Yeah, I built a franken-query function here.
As you can see, we have three custom query expressions in a single query function.

1. Let's look at `qbuilder`.
   This function decides whether it should return the equality of an empty `PersistField a`
   in an `Entity` when it receives a `Just a` value and returns `True` otherwise.
   Why did I decide that?
   Because based on my quick and dirty thinking (lol), in where clause, `True` is the
   "empty value" of a query.
   You know, something like `1 = 1` in `select * from users where 1 = 1`, yeah something like that.
2. `ordering` function.
   In FluxBB, they can query based on three different data types.
   `UsersUsername :: Text`, `UsersJoinTime :: UTCTime`, and `UsersRepliesPosted :: Int64`.
   I mean, we can't build a general function inside of `orderBy` when we have
   three different data type.
   So, instead of `ordering b u o = (chooseAscension b) (u ^. accessor)`, which will
   make GHC complaint about "rigid data type" or something like that,
   we should build a function which directly returns `query ()`.
   And here we are, we have one.
3. Practically the same hurdle with `ordering`, "rigid data type", when we only
   write `chooseAscension True = asc`.
   So, we should make the function more "concrete" by placing an `e` which is an `expr`.

Yeah, pretty much that's it, for the query explanation.

So, let's head for `UserListR`, write and choose something based on the default value
of the form, perhaps we will get some users which match with the criterions.

Anyway, here's the commit of this section: ["user data. search."](https://gitlab.com/ibnuda/Cirkeltrek/commit/092932cd9f2c174060eda9e158991a03daffc7ce)

#### Editing User's Information

Let's see, usually users want to change their password or email once in a while.
Assuming they have the inclination to do so, obviously.
There could also be times where user couldn't even login and admin should edit it
for them. Yeah, I know, being an admin is a pain in the back.
Oh yeah, unlike a normal user,  when admin want to change user's password, he could
just simply set it without putting the old password.
And I retract my previous statement about admin's pain.

So, as usual, we should create the handler first, followed by form, and then `Flux` module.
```
getUserEditR userid = do
  (uid, name, group) <- allowedToActuallyEdit userid -- [1]
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid -- [2]
  (wid, enct) <- _ group (usersEmail user) -- [3]
  profileLayout uid name group user' $(widgetFile "profile-info-edit")

```
You see, we couldn't know what to edit when we don't even know what is it, could we?
So that's `[2]` for you.
Because we don't want to be bothered by checking what kind of form we should display,
we just "outsource" it to another function.
Will back to it in a minute.
And yeah, we are using another modified function of `allowedToPost` for the `[1]` marked
function.
```
allowedToActuallyEdit pid = do
  (uid, name, group) <- allowedToPost
  if uid == toSqlKey pid || group == Administrator
    then return (uid, name, group)
    else permissionDenied "You're not allowed to edit user's information."

```
Isn't it beautiful? No actually.
We just simply passing around things like checking `userid` of the incoming request
and its `group` too.
You see, "only admin and user who could modify their informations."

And for the holed function `[3]` of `getUserEditR`, here it is.
```
generateFormEdit :: Grouping -> Text -> Handler (Widget, Enctype)
generateFormEdit Administrator = generateFormPost . editByAdminForm
generateFormEdit _             = generateFormPost . editByUserForm

```
Where their respective forms are just simply `passwordField` and `emailField`.
That function have no qualms because of the difference of the generated forms here
because both of them returns `(Widget, Enctype)`.
A bit different when running `generateFormPost`'s parsing counterpart.
We wouldn't be able to do that because they return different types of things.

Anyway, we've completed `GET` handler here.
The next step is creating `POST` handler.
```
postUserEditR userid = do
  (uid, name, group) <- allowedToActuallyEdit userid -- [1]
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid -- [1]
  case group of -- [2]
    Administrator -> do
      ((res, wid), enct) <- runFormPost $ editByAdminForm (usersEmail user) -- [3]
      case res of
        FormSuccess r -> do -- [4]
          let (newpass, email) = (editByAdminFormNewPass r, editByAdminFormEmail r)
          _ uid' newpass email -- [5]
          redirect $ UserR userid - [6]
        _ -> profileLayout uid name group user' [whamlet|Please fill the input correctly|]
    _ -> do
      ((res, wid), enct) <- runFormPost $ editByUserForm (usersEmail user) -- [3]
      case res of
        FormSuccess r -> do
          let (oldpass, newpass, email) = (editByUserFormOldPass r ,editByUserFormNewPass r ,editByUserFormEmail r)
          _ uid (usersPassword user) oldpass newpass email -- [7]
          redirect $ UserR userid - [6]
        _ -> profileLayout uid name group user' [whamlet|Please fill the input correctly|]

```
The function above is simple.
Too simple, even.

1. Same procedures with `GET` handler.
2. Dedices which form should we use to parse the form based on the group.
   Remember, admin could simply set a new password.
3. Simple form parsing.
4. Only proceed when the form is correctly formed.
5. Simple CRUD operation.
   Will write it later.
6. Redirect to user's page.
7. Simple CRUD operation.
   Will write it later.

Now, let's write function `[5]`
```
updateInfoByAdmin userid Nothing email = liftHandler $ runDB $ updateUserEmail userid email -- [1]
updateInfoByAdmin userid (Just np) email =
  liftHandler $ runDB $ do
    newpassword <- liftIO $ makePassword (encodeUtf8 np) 17 -- [2]
    updateUserEmail userid email -- [1]
    updateUserPassword userid (Just $ decodeUtf8 newpassword) -- [3]

```
I'm not really sure whether the function above is clear or not.
But I can assure you that `[1]` and `[3]` are just standard queries
with nothing interesting in particular.
Even `[2]` itself should have been talked when we look at `RegisterR`'s snippet.

For function `[7]` of `postUserEditR`, it's pretty hectic.
```
selfUpdateInfoByUser userid userpass oldpass newpass email = do
  case (userpass, oldpass, newpass) of
    (Nothing, _, _) -> error "Your profile is broken. Ask admin to fix this."
    (Just _, Nothing, Nothing) -> liftHandler $ runDB $ updateUserEmail userid email
    (Just _, Nothing, Just _) -> invalidArgs ["You cannot update your password without providing your old password."]
    (Just _, Just _, Nothing) -> invalidArgs ["You cannot use an empty password."]
    (Just up, Just op, Just np) -> do
      if verifyPassword (encodeUtf8 op) (encodeUtf8 up)
        then do
          newpassword <- liftIO $ makePassword (encodeUtf8 np) 17
          liftHandler $
            runDB $ do
              updateUserEmail userid email
              updateUserPassword userid (Just $ decodeUtf8 newpassword)
        else invalidArgs ["Your password don't match with the old one."]

```
The function above could be refactored, I guess.
But whatever, will do that next time (or never, lol).
So, what we have here? Oh, yeah, pattern match for passwords.
Basically simple thing.

- When the existing password in db is null, we should display error.
  Because it's impossible, to be honest.
- When there's password in db and user didn't fill password field, we
  should update their email.
  I mean, they didn't change their password, right?
  So their intention must be changing email, right?
- When there's password in db and new password field while nothing in old password,
  we should throw an invalid thingy.
- Same goes when there's no new password, we just simply throw n error.
- Finally, when the request provides everything, we just simply
  verify the password and then update email and password data in database.
  Else, just throw some error or something.

Current situation: [user data. change info.](https://gitlab.com/ibnuda/Cirkeltrek/commit/b9f4f6cc8d46926d6ae6e93c265bd4499550baf8)

#### User's Posts and Topics

Now, it feels incomplete when we can see user's information and even edit it, but
we couldn't even read his activities.
Don't worry, we are going to fix that.
Let's assume that "activities" here is his posts and topics history, for the sake
of simplicity.
Now, we will see his post history first.
```
getUserPostsR userid = do
  (uid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  posts <- getUserPosts $ toSqlKey userid
  profileLayout uid name group user' $(widgetFile "profile-info-posts")

```
Yeah, that simple. 
First, only logged in users are allowed to see this page.
Then we get the user's information for the sake of `profileLayout`
and then, we should get those posts made by the corresponding user.
Simple.

For user's topic history, it doesn't differ much.
```
getUserTopicsR userid = do
  (uid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  topics <- getUserTopics $ toSqlKey userid
  profileLayout uid name group user' $(widgetFile "profile-info-topics")

```
You see, it's practically the same function with `getUserPostsR`.
The only difference here is `topics` doesn't only contain `[Entity Topics]` but also
contains the name of the forum which the topic being posted at.
Why did I do that?
Simply because I want to show it.

And I'll break this nice and easy section with stupid correction.
Whenever a user posts a reply or create a new topic, his post count didn't
increment.
So, I have modified `createTopicByPosting` and `replyTopicByPosting` by adding
```
updateUserIncrementTopic userid
```
and
```
updateUserIncrementPost userid
```
to the existing functions' bodies.
Where `updateUserIncrementTopic` and `updateUserIncrementPost` themselves are just
queries that increment `UsersTopicsCreated` and `UsersRepliesPosted`, respectively..

Commit: [here](https://gitlab.com/ibnuda/Cirkeltrek/commit/b0f9f864faccc67869a27404ab75af161a9b3724)

## Finish, I guess.

That's all, folks.
This article has more words than my theses.
I don't think I can write any longer about it.
Furthermore, I think this project can be considered as a fully functioning forum software.
So, yeah, it was fun while it lasts.

# Lessons Learnt

- Dumping snippets as blog post content makes me feel bad.
- English is hard.
- Writing sentences coherently especially so.
- Short circuiting functions are nice.

# Exercises For The Readers

- I don't know, perhaps write custom group?
- Or even loosen the permissions so any anon-kun can read the discussions?
- Even more intriguing, forum for mods and/or admins, maybe?
