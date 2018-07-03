Thinkster's RealWorld - Backend
2018-07-02 15:00:13.441855576 UTC
Post

(WIP)

## Background
Nothing much, I just want to re-visit [Servant](http://haskell-servant.github.io/)
and [servant-auth](https://github.com/haskell-servant/servant-auth/) and how does
it feel compared a half year ago.
Other than that, I feel empty, man!

Anyway, my first try implement [Thinkster](https://thinkster.io)'s
[Conduit API](https://github.com/gothinkster/realworld) (lol, "API") can be found
at [RealWorldTM](https://gitlab.com/ibnuda/RealWorldTM) which I think should be
fully functional.
But, it doesn't feel right if I don't write it up.
So, here we go again.

Oh, by the way, perhaps you want to read my previous post about
[authorization in servant](2017-11-03-authorization-in-servant.html) where I rambled
too much about servant previously.

## Prep
As stated previously, in this article I will use:
- [servant](https://hackage.haskell.org/package/servant)
- [servant-auth](https://hackage.haskell.org/package/servant-auth)
- [esqueleto](https://hackage.haskell.org/package/esqueleto)
- [persistent-postgresql](https://hackage.haskell.org/package/persistent-postgresql):
  Because I really like `array_agg`.

And unlike what I did in back then, when I use mutable global variable, I will use
`ReaderT` pattern properly here.
Thanks for the [example](https://github.com/parsonsmatt/servant-persistent/), Matt!

Now, open your terminal and create a servant project using `stack`.
```
stack new real-world-conduit protolude
```
Yeah, I use `protolude` just because I like it.
Followed by editing `stack.yaml` and `real-world-conduit.cabal` so it will look
like in the commit.

There are a few dependencies that I feel I should give you some explanations:
- `aeson-casing`: I really like this package.
- `bcrypt`: I guess it's a common wisdom to never store your password in a plaintext.
- `foreeign-store`: I really like Yesod's scaffold's `yesod-devel` thingy. Really nice, if you ask me.
  And to have that feature, the `DevelMain.hs` imports `Foreign.Store`.
- `monad-logger`: `persistent` asks an access to logger whenever it executes a query.
- `regex-compat`: because I don't like capital letters and special characters.

As usual, I will put the commit links for whatever I do here.
By the way, here it is: [initial commit](https://gitlab.com/ibnuda/real-world-conduit/commit/bfc727e29c5af755a0e41da7b4f5dd60f39badb3).

## Request and Response Types.
When you read RealWorld's [api spec.](https://github.com/gothinkster/realworld/tree/master/api),
you will see that there are a bunch of request and response schemas.
I, for one, am really forgetful person.
I can hardly remember which is which, so I prefer to define those schemas first
and let the compiler helps me recognise it.

I've copied the route, json schemas, and stuff into `readme.md` file of this project
so, it will shorten this article for a lot.
Please check that out.

Basically, both the response and requests payloads look like the following:
```
{
  "nameofpayloadkind": {
    "fielda": "valuea",
    "fieldb": "valueb"
  }
}

```
And that means we have to (or I want to, to be exact) make two records for a single request type.
Example given:
```
{ "user":{ "email": "email@domain.tld", "password":"strong pass", "username": "emanresu" }}
```
Will be represented by:
```
data RequestRegistration = RequestRegistration
  { reqregUser :: RequestRegistrationBody
  } deriving (Generic)
instance ToJSON RequestRegistration where
  parseJSON = genericParseJSON (aesonPrefix camelCase)
data RequestRegistrationBody = RequestRegistrationBody
  { reqregbodyUsername :: Text
  , reqregbodyEmail    :: Text
  , reqregbodyPassword :: Text
  } deriving (Generic)
instance ToJSON RequestRegistrationBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)
```
Above, those two records represent a single json schema (for `reqregister.json` in `readme.md`
file).
And please note that the parts from the field which started by capital letters are the
fields which will be used for the json payload's fields.
There are also `deriving (Generic)`s because we will not write our own parse rules for json
(de)serialisations.

Current commit status: [types](https://gitlab.com/ibnuda/real-world-conduit/commit/d708fd2aa9c6e04ddc50beb733225274288794c4).

## Database Representation.

I have decided how the database modeling looks like for this project.
I don't know how normalised it is, but I'm pretty sure Mrs. Dyah will not be
disappointed by it.
You can read see the tables on [note on joins](https://siskam.link/2018-07-01-note-on-joins.html).
Although I omitted two tables there (`follows` and `comment`), I will make it up by showing
`persistent`'s database model thingy.
```
share [ mkPersist sqlSettings , mkDeleteCascade sqlSettings , mkMigrate "migrateEverything" ] [ persistLowerCase|
    Follow sql=follows
      followerId UserId
      authorId UserId
      UniqueFollowerAuthor followerId authorId
      deriving Generic
    Comment sql=comments
      body Text
      createdAt UTCTime
      updatedAt UTCTime Maybe
      articleId ArticleId
      userId Userid
      deriving Generic
  ]

instance ToJSON User where
  toJSON = genericToJSON (aesonPrefix camelCase)
instance FromJSON User where
  parseJSON = genericParseJSON (aesonPrefix camelCase)
instance ToJWT User
instance FromJWT User
```
I guess that the intentions for that `share` block is pretty clear for it represents
`follows` and `comments` tables at the database.
But why did I create `ToJSON` and `FromJSON` instances when I can do that in the `share`
block previously?
Pretty much the same reasons why I did that at the previous section.
And why did I also create `ToJWT` and `FromJWT` for `User`?
That because we are going to use JWT for auth in this project.

Current commit: [persistent model](https://gitlab.com/ibnuda/real-world-conduit/commit/0a14c4f30e5724edf6d9801696972faf44d96e39).

## `Handler`, `ReaderT`, and `AppT`.
I actually don't really like talking about types a lot.
But, in this case, I think that `servant`'s `Handler` doesn't really have the
capabilities we want.
For example, we don't want to pass around a bunch of parameters or even use global thingy.
We also want to use a single source of configuration.

To solve that, the most commonly used pattern in this kind of application is the
`ReaderT` pattern where we create a type which derives `MonadReader a`, where `a`
usually is a data.
So, instead of something like this:
```
getArticle :: Configuration -> Pool backend -> Handler Article
getArticle conf pool = do
  let q = undefined :: ReaderT backend IO Article
  return $ runSqlPool q pool
```
We can use:
```
getArticleTastier :: (MonadReader Configuration m) => m Article
getArticleTastier = do
  let q = undefined :: ReaderT backend IO Article
  conf <- asks myConfiguration
  pool <- asks myPool
  return $ runSqlPool q pool
```

And to give you reasons why I do feel that `servant`'s `Handler` can't do that, I will
show you a few things.
Here's `Handler`'s signature:
```
newtype Handler a = Handler
  { runHandler' :: ExceptT ServantErr IO a
  } deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServantErr, MonadThrow, MonadCatch)
```
As you can see, there's no `MonadReader` instance there.
Sure, we can create our own `MonadReader` instantiation but I'm afraid things will
go out of hands real fast. 

Fortunately, there's a helper function from `servant-server` to "transform" from a
`ServerT api m` to `ServerT api n`.
And in this case, that function, `hoistServer`, will transform our `CustomServer`
into `Server`.
Which ultimately, `Server` is the one which will be `run` after being transformed
into `wai`'s `Application`.
From our `CustomServer`:
```
ServerT ourapi CustomHandler
               -- ^ This one.
```
To `servant`'s `Server`:
```
ServerT ourapi Handler
```

### Creating Custom Handler.
Remember that we are going to create a custom handler just because we can't hold
a configuration data.
But, we don't have any data to hold at the moment.
Better create it right now.

```
-- src/Conf.hs
data Configuration = Configuration
  { configurationPool        :: ConnectionPool -- ^ database connection.
  , configurationJWTSettings :: JWTSettings    -- ^ pretty clear.
  }
```
That's it, I guess.
I couldn't think any other data that should be carried over.

The next step is creating our custom `Handler` and we will be calling
it as `Coach`.
That's what I got from asking DuckDuckGo, by the way.

![alt text](images/handlersynonym.png "Handler synonym.")

```
newtype CoachT m a = CoachT
  { runCoach :: ReaderT Configuration (ExceptT ServantErr m) a
  } deriving (Functor, Applicative, Monad, MonadReader Configuration, MonadError ServantErr, MonadIO)
type Coach = CoachT IO
```
Please note that the main difference between `runHandler'` and `runCoach`
is, basically, `runHandler'` is being wrapped by `ReaderT Configuration`.
Other than that, there's difference.
And why do we didn't we derive `MonadThrow` nor `MonadCatch`?
I don't know, I can't find why. If you know, please tell me.

Now, to transform from a `Coach` to a `Handler` (so we can have a `Server`)
we have to have a function to do so.
Let's write that.
```
coachToHandler :: Configuration -> Coach a -> Handler a
coachToHandler conf coach = Handler (runReaderT (runCoach coach) conf)
                                     -- ^ a          ^
                                                  -- + b
```
A little explanation why did we what we did:

1. We extract `ExceptT ServantErr m` so it can be transformed to `Handler`.
   (or "extractor runner function").
2. We get the reader transformer.
   (or "extractor function")
3. Then `Handler` transform the result from point a.

Current commit: [configuration and custom handler](https://gitlab.com/ibnuda/real-world-conduit/commit/5e1fa9322613570546a7658ef96cfb7eabce034f).

## Building the Interface.

After the preparation is complete, we can now start building the server.
We can start by defining the main api as the following
```
-- Don't forget to use DataKinds extensions and friends.
type ConduitAPI auth =
  "api" :> (Servant.Auth.Server.Auth auth User :> UserInformationAPI)
  :<|> "api" :> UserAdministrationAPI
  :<|> Raw
```
Here, we defined that in path `/api/{path/defined/by/UserInformationAPI}` is
being guarded by authorization while `api/{path/defined/by/UserAdministrationAPI}`
is not.

I guess it's the right time to create the `UserInformationAPI`

### Creating `UserInformationAPI` and `UserAdministrationAPI`

Next, we should define `UserInformationAPI` in `API/User.hs`.
```
type UserInformationAPI =
  "user" :> Get '[ JSON] ResponseUser
  :<|> "user" :> ReqBody '[ JSON] RequestUpdateUser :> Put '[ JSON] ResponseUser
type UserAdministrationAPI =
  "users" :> ReqBody '[ JSON] RequestRegistration :> Post '[ JSON] ResponseUser
  :<|> "users" :> "login" :> ReqBody '[ JSON] RequestLogin :> Post '[ JSON] ResponseUser
```
Basically, path `/user`, accepts `GET` request and will give `ResponseUser` as
the reply.
Not only that, `/user` will also accept `PUT` request with the payload of `RequestUpdateUser`.
So yeah, basically those two defined route point at the same route but accepting
different kind of request.
As for the reason, I dislike definition like the following:
```
type UserInformationAPI =
  "user" :>
    ( Get '[ JSON] ResponseUser
    :<|> ReqBody '[ JSON] RequestUpdateUser :> Put '[ JSON] ResponseUser)
```
Yes, the former and the latter are basically the same.
But it feels so wrong putting it like that.

After defining the route and/or api, we should create the something that makes them
usable.
In this case, we should create `ServerT` which reads `UserAdministrationAPI` and
`UserInformationAPI` as its route and will use `CoachT IO` as its `Handler` replacement.
Why do we do that?
Remember, folks. `Handler` has no `MonadReader Configuration` while we are going
to use that `Configuration` a lot because of we need to talk to the database.

```
userInformationApi :: MonadIO m => AuthResult User -> ServerT UserAdministrationAPI (CoachT m)
userInformationApi authres = panic ""

userAdministrationApi :: MonadIO m => AuthResult User -> ServerT UserAdministrationAPI (CoachT m)
userAdministrationApi authres = panic ""

userInformationProxy :: Proxy UserInformationAPI
userInformationProxy = Proxy

userAdministrationProxy :: Proxy UserAdministrationAPI
userAdministrationProxy = Proxy
```
Don't ask me why I should define `userInformationProxy` and `userAdministrationProxy`
which basically just `Proxy` in disguise?
I don't really know.
But my gut feeling tells me that I really need it so `hoistServer` will be able
to make a `ServerT OurAPI Handler` from `ServerT OurAPI Coach`.

And the reason why I decided to put `panic ""` at `userInformationApi` and
`userAdministrationApi` because we will revisit it later when all of the REST interface
has been written.
For now, we have to make it compile first.

Okay, no we have to transform our `userInformationApi` to `Server UserAdministrationAPI`.
```
userAdministrationServer :: Configuration -> Server UserAdministrationAPI
userAdministrationServer configuration =
  hoistServer
    userAdministrationProxy
    (coachToHandler configuration)
    userAdministrationApi
userInformationServer :: Configuration -> AuthResult User -> Server UserInformationAPI
userInformationServer configuration authres =
  hoistServer
    userAdministrationProxy
    (coachToHandler configuration)
    (userInformationApi authres)
```
There's a difference when it comes to the parameter.
If you look at the `ConduitAPI` definition above, there's a type definition that
preceded by `Servant.Auth.Server.Auth auth`.
That what makes `userInformationApi` receives one extra parameter compared to
`userAdministrationApi`.

### Make It Run!

Now, we are editing `RealWorld.hs` again.
Here, we should create a `Server` from `ConduitAPI` so it can be run
and make some helper functions so it has the capabilities like what `yesod --devel`
has.

```
conduitProxy :: Proxy ConduitAPI
conduitProxy = Proxy

conduitServer :: Configuration -> Server (ConduitAPI auth)
conduitServer conf =
  userInformationServer conf
  :<|> userAdministrationServer conf
  :<|> serveDirectoryFileServer "front"
```
Unlike what we wrote in `userAdministrationApi` which return an error, here we
combine `Server UserAdministrationAPI`, `Server UserInformationAPI`, and `Server Raw`
using this cute fish combinator `:<|>`.

And now it's the main part time, letting it run!

```
running :: IO ()
running = do
  jwk <- generateKey -- [1]
  pool <- runStderrLoggingT (createPostgresqlPool connstring 10) -- [2]
  let jws = defaultJWKSettings jwk
      cfg = defaultCookieSettings :. jws :. EmptyContext
      conf = Configuration pool jws
  runSqlPool doMigration pool -- [3]
  run 8080 (serveWithContext conduitProxy cfg (conduitServer conf)) -- [4]
```

1. Standard JWK generation.
2. Database pool creation.
3. Migration, using our recently created pool.
4. Using the proxy, context config, and our server, we run it at port 8080.

That's it, folks!!!

Current commit: [started to define the rest interface](https://gitlab.com/ibnuda/real-world-conduit/commit/9c4299b1d7a644129b111640f7e675e43d4efeff).

##### Note
I use a lot of "feel" word when I write this because I'm pretty sure that when I do it,
I haven't had a decent experience and/or knoweldge to back up the thing I do.