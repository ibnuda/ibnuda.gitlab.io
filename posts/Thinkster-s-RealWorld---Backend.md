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
- `foreign-store`: I really like Yesod's scaffold's `yesod-devel` thingy. Really nice, if you ask me.
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
I don't know how normalised it is, but I'm pretty sure Mrs. Dian will not be
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

## `Handler`, `ReaderT`, `CoachT`, and `ServerT`.
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
From our `CustomServer`:
```
ServerT ourapi CustomHandler
               -- ^ This one.
```
To `servant`'s `Server`:
```
ServerT ourapi Handler
```
And in this case, that function, `hoistServer`, will transform our `CustomServer`
into `Server`.
Which ultimately, `Server` is the one which will be `run` after being transformed
into `wai`'s `Application`.

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
Here, we defined that in path `/api/{path/by/UserInformationAPI}` is
being guarded by authorization while `api/{path/by/UserAdministrationAPI}`
is not.
Compared a while ago when I used `Servant.Experimental.Auth`,
`servant-auth` is much nicer.
All I do is just putting `Auth auth entity` preceding the API type
I want to protect.

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

Okay, now we have to transform our `userInformationApi` to `Server UserAdministrationAPI`.
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

### Making It Run!

Now, we are editing `RealWorld.hs` again.
Here, we should create a `Server` from `ConduitAPI` so it can be run
and make some helper functions so it has the capabilities like what `yesod --devel`
has.

```
conduitProxy :: Proxy (ConduitAPI '[ JWT])
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
It's worth nothing that `ConduitAPI` which being used as `conduitProxy`'s
parameter has `'[ JWT]` as its own parameter.
It signifies that we are using JWT as our main means of authorisation.

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

That's it, folks!
You can try to run it inside of your `ghci` and head to [http://localhost:8080/api/user](http://localhost:8080/api/user)!
It will show you something really nice there!

Current commit: [started to define the rest interface](https://gitlab.com/ibnuda/real-world-conduit/commit/9c4299b1d7a644129b111640f7e675e43d4efeff).

### Finishing REST Interface definitions.

I won't write too much because basically what I did here is the same as the 
previous section.

Current commit: [finished api definitions and uses RealWorld.hs instead of Lib.hs as the entry point](https://gitlab.com/ibnuda/real-world-conduit/commit/9f983d89207d6ec1cd381dfe9188edea4a5ddc92).

## Building `Coach`es.

Yeah, we are going to create our modified `Handler`s (`Coach`) here.
To remind you, `Handler` is where we process the requests and perhaps return the
desired response.
For example, the simplest server in this project, `tagServer`, only has a single
`Handler` which returns `ResponseTags` in form of json when there's a request coming
to `/api/tags`.

But, first thing first.
We should create `yesod-devel` thing first.

### Copying `DevelMain`.
It's an optional step, actually.
I just like it soo much.

Don't worry too much about it.
All I did was copying `DevelMain` from `app/` directory from `yesod-postgresql`
template and then modify it a bit.

Don't forget to create a `Makefile` or an alias command that contains:
```
ghcid --command "stack ghci real-world-conduit" --test "DevelMain.update"
```
Hereon, whenever you change your code, you will see the result in real time.
That's nice, no?

Commit: [added ghcid](https://gitlab.com/ibnuda/real-world-conduit/commit/89a2798c64a18e6f3500de886000b16f47c3de23).

### Building `Coach` for `tagsServer`.
We create it first because `tagsApi` basically just serving a single
simple `GET` request.
Not only that, it also doesn't need an authentication so it could be a perfect
example.

First, we should create a directory named `Coach` in `src` directory.
And then create a file named `Tags.hs` and fill it with
```
getTagsCoach :: MonadIO m => CoachT m ResponseTags
getTagsCoach = 
  tags <- fromdb -- owo whats dis?
  return $ tagsToResponse tags
  where
    tagsToResponse xs = ResponseTags $ map (tagName . entityVal) xs
```
The code above basically just queries the database and then transform
the fetched rows into a `ResponseTags`.
Also, please note the signature of the function above.
We are not using `Handler ResponseTags` like we usually do in `servant`
scaffolded template.

Oh, we still don't have any ways to talk to the database.
Better fix that now.
Let's create a file named `Tags.hs` (yeah, I'm not a creative dudu)
in `Que` folder.
And put standard esqueleto query.
```
selectTags = do
  select $ from $ \tag -> do
    orderBy [asc (tag ^. TagName)]
    return tag
```
And then create a query runner inside of `Model.hs`
```
runDb :: (MonadReader Configuration m, MonadIO m) => SqlPersistT IO b -> m b
runDb q = do
  pool <- asks configurationPool
  liftIO $ runSqlPool q pool
```

And then followed by replacing a line in `Que.Tags` module, from `tags <- fromdb`
with `tags <- runDb selectTags`.
Furthermore, you should also replace a line in `API.Tags` module from `panic ""`
to `getTagsCoach`

Finally, run it and head to [our recently defined rest interface](http://localhost:8080/api/tags)!!ELEVEN1!

Current situation: [finished /api/tags coach](https://gitlab.com/ibnuda/real-world-conduit/commit/53c9063e85be867062d4e9927b89e8134084bc21).

### Building `Coach`es for `userAdministrationServer`.
Another server which doesn't need an authentication is `userAdministrationServer`.
Not only that, it is also the one which being used to register and login.
So, it's perfect for us to continue this activity.

First, we should create the coach for user registration.
Of course we are going to create a file in the `Coach` directory first and put
a file named `Users.hs` there.

Of course, there are a few things that should be considered when a registration request
comes (simplified too much).

1. What should the server send when it has complied to the request?
2. What should the server send when it couldn't comply to the request?
3. What should be considered when the server want to process the request?

In the RealWorld's [api spec.](https://github.com/gothinkster/realworld/tree/master/api),
there's no clear cut answere.
So, let's do whatever we see fit.

1. As defined in the `Model.hs`, there could only be one for each username
   and email.
2. So, when a request comes with conflicting username and/or email,
   we should return a 422 error status.
3. When the request comes with non-conflicting username and email,
   we should process them and returns 200 ok status.
   (Although I'm not sure if 200 is the correct one, because there's a created
   request, and 201 is the correct one about it.

Now, let's write it out.
```
postRegistrationCoach (RequestRegistration reqreg) = do
  uniqueenough <- checkingUsernameEmail reqreg
  hashedpass <- hashingpass
  userentity <- insertingUserEntity reqreg
  createuserresponse
  return userresponse
```
There are at least 4 steps that need to be done.
Okay, let's start by creating a function that checks username and email which
returns 422 when the request is not unique enough.
```
usernameEmailDoNotConflict username email = do
  existings <- runDb (selectUsernameEmail username email) -- [1]
  case existings of
    [] -> return ()
    _  -> throwError err422
```
where the function that being referred as `[1]` is just a standard esqueleto select
which equivalent to `select * from users where username = ? or email = ?;`.

The next step is hashing the password in case of the request is unique enough.
Although I feel that hashing the password first is better than checking the
username and password first in term of limiting the database query, I've already
written the previous paragraph.
So yeah, I made mistakes.

The next part of that hashing the password itself.
```
generatePassword password = do
  mpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (Char8.pack $ Text.unpack password)
  case mpass of
    Nothing -> generatePassword password
    Just pa -> return pa
```
Which basically keep repeating the hash until the host os generating enough entropy
or something like that so whatever happens, it will return a hashed password.
But, I don't really know about that, actually.
Check it out yourself [BCrypt.Crypto](https://www.stackage.org/haddock/lts-11.15/bcrypt-0.0.11/src/Crypto.BCrypt.html), man!

Following that, we are going to create the JWT which will be used for auth in the future
incoming request by this request.
Well, whatever, you know what I mean, I'm sure.

```
generateToken user = do
  jws <- asks configurationJWTSettings
  etoken <- liftIO $ makeJWT user jws Nothing
  token <- eitherToCoach etoken (decodeUtf8 . BL.toStrict) err500
  return token
eitherToCoach (Left x) _ onFail = throwError $ onFail {errBody = show x}
eitherToCoach (Right v) onSuccess _ = return $ onSuccess v
```
`generateToken` above just creates the token which basically will never expires.
One thing, though.
There's a helper function which throws error on, well, error.

Current situation: [finished postRegistrationCoach](https://gitlab.com/ibnuda/real-world-conduit/commit/4ed12b16a43cd5eff5c42e6a4687c814d2c4c754).

The next part is creating `Coach` for the second part of `userAdministrationApi`
which is handler for post request for login.
Let's create that.

```
postLoginCoach (RequestLogin (RequestLoginBody email password)) = do
  checkingExistenceOfEmail -- [1]
  validatingPassword -- [2]
  generatingToken -- [3]
  returningResponse -- [4]
```
The step of authenticating user is like the step above, I guess.
At the first step, we should return a 404 error when there's no user who use that
email.
At the second step, we should return a 401 error when the provided password cannot
be verified using the existing hashed password.
While the third and fourth step is basically the same as the previous handler,
generating token andn returning response.

Let's create the first step.
But, don't forget that we have made `email` field in the database as a `UNIQUE`
field in the database, we can easily use that.
```
postLoginCoach .... =  
  (Entity uid user) <- notFoundIfNothing =<< runDb (getBy (UniqueEmail email))
  ... 

where
  notFound Nothing = throwError $ err404 { errBody = "No such thing." }
  notFound (Just x) = return x
```
Pretty simple actually.
And I'm still looking for the function name of `bind` function which is not an
operator (`>>=` and/or `=<<`).

And for validating password, we can use `unless`.
```
unless
  (validatePassword
    ((pack . unpack) (userPassword user))
    ((pack . unpack) (password user)) $
  throwError err401 { errBody = "No such thing." }
```
This part is pretty simple actually, `unless` the result of `validatePassword`
is `False`, we would not throw a 401 error.
Which basically the password is correct.
Also, please note that that `pack` above is from `Data.ByteString.Char8` while `unpack`
is from `Data.Text`.
Why? Because `validatePassword` receives `ByteString` while our `password`s are
`Text`.

For the third and fourth step, I guess we can just shift `generateToken` and
`eitherToCoach` a bit left so those functions can be reached from other functinos
as well.
While the last step, it's just standard return.
Nothing to worry here.

Of course you should also modify `userAdministrationApi` so it will look like the
following:
```
userAdministrationApi = postRegistrationCoach :<|> postLoginCoach
```

Okay, now you can try to login using `curl` or some other fancy tools to send
some request to [login path](http://localhost:8080/api/users/login).

PS: I was wrong for saying "conflicting resource == 422" at the first part of
this section.
The correct one should be 409.

Our status: [Finished creating `postLoginCoach` and fixing error code](https://gitlab.com/ibnuda/real-world-conduit/commit/26c42523d92e6f7f95487369a22afdb581eda0a9)

### Building `Coach`es for `userInformationServer`.
This server is consisted from two handler.
The first one, is where we get our current user's information which could also
be used to renew their JWT.
The second one is where we update our user's information.

#### Building `getUserInformationCoach`.
To be really honest, this part is practically a token renewal route.
We only re-generate the token and then return it.
But, only requests with valid JWT which can be processed.

```
getUserInformationCoach (Authenticated user) = do
  token <- generateToken user
  return $ UserResponse $ UserResponseBody (userEmail user) (Just token) (userUsername user) (userBio user) (userImage user)
```
Yeah, that's it.
Just token generation and return it.

#### Building `putUserInformationCoach`.
Well, this one is a bit cumbersome, I guess.
There are a few thing that should be considered.

1. A user could not update their username and/or email as the same username and/or email
   of the other users.
2. When a user does that, I think error 409 is the correct response for them.
3. When a user tries to update his username and/or email with his old username and/or email,
   the server should ignore it.

After those considerations, the next step is getting their new information which
could be accomplished by fetching it from the database.
But, how would we know which email should be used in case of that user decided
to change it and the previous step approved the decision?
Well, hold my beer. (and please endure this round-trip and shitty code)

```
putUserInformationCoach (Authenticated user) (RequestUpdateUser requpdate) = do
  let newUsernameEmail old (Just x) = if x == old then Nothing else Just x
      newUsernameEmail old Nothing = Nothing
      -- ^ [1]
      perhapsnewusername = newUsernameEmail (userUsername user) (requpdtuserbodyUsername requpdate)
      -- ^ [2]
      perhapsnewemail    = newUsernameEmail (useremail    user) (requpdtuserbodyemail    requpdate)
      -- ^ [2]
  existings <- runDb $ selectUserByMaybeUsernameEmail perhapsnewusername perhapsnewemail
                      -- ^ [3]
  unless (null existings) $ throwError err409 {errBody = "Already used."}
  runDb $
    updateUser -- [4]
      (userUsername username)
      (requpdtuserbodyEmail requpdate)
      (requpdtuserbodyUsername requpdate)
      (requpdtuserbodyPassword requpdate)
      (requpdtuserbodyImage requpdate)
      (requpdtuserbodyBio requpdate)
  (Just (Entity _ u)) <-
  -- ^ [5!!!!]
    runDb $ getBy $ UniqueEmail $ fromMaybe (userEmail user) perhapsnewemail
  token <- generateToken u
  return $ UserResponse $ UserResponseBody (userEmail u) (Just token) (userUsername u) (userBio u) (userImage u)
```
Do you see this shitshow? I do.

1. This function complies with the consideration number 3 above.
2. It could be our new username and/or email, I guess?
   As long as we believe the result of the previous point.
3. It's another query, which look like the following:
```
selectUserByMaybeUsernameEmail musername memail = do
  select $ from $ \user -> do
    where_ (whereBuilderOr musername user UserUsername
            ||. whereBuilderOr memail user UserEmail)
    return user
  where
    whereBuilderOr Nothing _ _ = val False
    whereBuilderOr (Just x) entity accessor = entity ^. accessor ==. val x
```
   There's nothing interesting with the query itself which basically translated to
   ```
   select * from users where username = ? or email = ?;
   ```
   when both of the `memail` and `musername` are not `Nothing` but it will become
   ```
   select * from users where username = ? or false;
   ```
   when `memail` is `Nothing`.

4. This one. This query is pretty ugly.
```
updateUser username memail musername mpassword mimage mbio = do
  mpassword' <- liftIO $ mapM generatePassword mpassword
  update $ \user -> do
    set user [ updateByMaybe memail user UserEmail
             , updateByMaybe musername user UserUsername
             , updateByMaybe mpassword' user UserPassword
             , UserImage =. val mimage
             , UserBio =. val mbio
             ]
    where_ (user ^. UserUsername ==. val username)
  where
    updateByMaybe (Just x) _ accessor = accessor =. val x
    updateByMaybe Nothing entity accessor = accessor =. entity ^. accessor
```
   You see this thing? That `updateByMaybe`?  When there's nothing to update, we
   just overwrite the old value with the old value itself.
   I don't know man.
   But if it is the best thing I could come up with, I guess I'm still a stupid kid.

5. Yes, you see that right? Basically we are ignoring the safety of Haskal's `Maybe`
   by casting it straight to `Just`.
And the rest is exactly the same with `getUserInformationCoach`.

Of course you need to update the definition of `userInformationApi` to
```
userInformationApi :: MonadIO m => AuthResult User -> ServerT UserInformation (CoachT m)
userInformationApi authres = getUserInformationCoach authres :<|> putUserInformationCoach authres

```
And then you can run it!

I swear, compared to half year ago when I'm using `Servant.Auth.Experimental`
thingy, `servant-auth` eases my life by a huge margin.

Where we are? [finished putUserInformationCoach](https://gitlab.com/ibnuda/real-world-conduit/commit/16197c86ca317f57871d650e0785d31963bb17a2)

### Building `Coach`es for `userProfileApi`.
Based on the defined interface, there are three handlers that need to be had.
A handler which serves user profile requests, and handlers which serve
creation and deletion of follow resource.
While the user profile handler consider auth as an optional feature,
the other two will ask auth.

#### `getUserProfileCoach`.
This coach is the one which serves the requests for users' profile.
There's nothing really interesting here, but the coach itself is
But, basically, we are having roundtrips and weird queries based on what
I consider the good way to check a user's existence.

1. Check the requested username's existence in the database.
2. When there's nothing like that in the database, that means
   we have to throw a not found error.
3. When there's something like that, we are going to check
   the existence of that user, AGAIN, and then check whether
   the one who requested the profile following him or not.
4. Return the result, based on the pre-defined response type.

Like this, my dude.
```
getUserProfileCoach authres profilename = do
  checkingexistence & throwifdoesn'texist
  gettheprofileandfollowstatus
  return
```
For `checkingexistence`, is pretty simple I guess.
```
  muser <- runDb $ getBy $ UniqueUsername profilename
```
Which will be used by `throwifdoesn'texist`
```
  when (isNothing muser) $ throwError err404 {errBody = "No such profile."}
```
Then, we are getting the profile and the following status of the user
and the requested profile.
```
  followings <-
    runDb $
    selectFollowsByUsernameAndProfilename
      (userUsername <$> authresToMaybe authres)
      profilename
  .....
  where
    authresToMaybe (Authenticated x) = Just x
    authresToMaybe _                 = Nothing
```
Why do we need to transform `AuthResult a` to `Maybe a`, you say?
Basically, if what matters in this scenario is the value of `a`
itself, which is a `User`.
Then, we will write `selectFollowsByUsernameAndProfilename`.
Again, the query that will be written is a bit long winded.
```
selectFollowsByUsernameAndProfilename (Just username) profilename = do
  select $ from $ \profile -> do
    let isfollowing =
          case_ [ when_ (exists $ from $ \(user, follows) -> do
                            where_ (follows ^. FollowFollowerId ==. user ^. UserId)
                            where_ (follows ^. FollowAuthorId ==. profile ^. UserId)
                            where_ (user ^. UserUsername ==. val username)
                            where_ (profile ^. UserUsername ==. val profile))
                        (then_ $ val True)
                ]
                (else_ $ val False)
    where_ (profile ^. UserUsername ==. val profilename)
    return (profile, isfollowing)
selectFollowsByUsernameAndProfilename (Just username) profilename = do
  select $ from $ \profile -> do
    where_ (profile ^. UserUsername ==. val profilename)
    return (profile, False)
```
Which basically checks the existence of `users.id` of `username` and `users.id`
of `profilename` in tables `follows` when the first parameter is not null (which
the parameter itself came from `authresToMaybe`).
If their `id`s are in the same row of `follows` table in the database, then
the value of `isfollowing` should be set to `val True`.
Else, it should be set to `val False`.
And the rest is just standard row fetching.

Back to `getUserProfileCoach`.
`followings` now is a data which has type of `[(Entity User, Value Bool)]`
so we can try to pattern match them.
```
  case followings of
    [] -> throwError err410
    ((Entity _ user, Value follow):_) -> do
      return $ ResponseProfile $ ResponseProfileBody (userUsername user) (userBio user) (userImage user) follow
```
Why do we use 410 as the status when there's nothing of value from the query?
Well, because a few moments ago, there are users whose names are `username` and
`profilename` but now, there's none.
So, 410 it is!
But when there's something of value, we should return that as the appropriate
response for the request.

And basically that's it for the current `Coach`.
We should put it in the `userProfileApi`
```
userProfileApi authres = getUserProfileCoach authres :<|> panic ""
```
Run it, dude!

#### Building `postUserFollowCoach` and `deleteUserFollowCoach`.
Practically, these two handlers are the same, structurally.
Let's see.
```
postUserFollowCoach :: MonadIO m => AuthResult User -> Text -> CoachT m NoContent
postUserFollowCoach (Authenticated user) profilename = do
  follows <- runDb $ selectFollows (userUsername user) profilename
  unless (null follows) $ throwError err409 {errBody = "Already followed that profile."} -- [1a]
  runDb $ insertFollows (userUsername user) profilename -- [2a]
  return NoContent
postUserFollowCoach _ _ = throwError err401

deleteUserFollowCoach :: MonadIO m => AuthResult User -> Text -> CoachT m NoContent
deleteUserFollowCoach (Authenticated user) profilename = do
  follows <- runDb $ selectFollows (userUsername user) profilename
  when (null follows) $ throwError err409 {errBody = "You are not following that user."} -- [1b]
  runDb $ deleteFollows (userUsername user) profilename -- [2b]
  return NoContent
deleteUserFollowCoach _ _ = throwError err401
```

As you can see at the marked numbers above, especially when there are a letter.
That's where the differences lay.
1. `post` handler will throw error when a user already follow the other user.
    While `delete` handler will throw error when a user didn't already follow another user.
2.  `post` handler will insert follow while delete will, well, delete the rows.
3. Basically that's it.
4. Also, don't forget to check the existence of the target profile first.

Anyway, the function for `selectFollows`, `insertFollows`, and `deleteFollows` are just like the following:
```
selectFollows username profilename = do
  select $ from $ \(user `InnerJoin` follow `InnerJoin` profile) -> do
    on (follow ^. FollowAuthorId ==. profile ^. UserId)
    on (follow ^. FollowFollowerId ==. user ^. UserId)
    where_ (user ^. UserUsername ==. val username)
    where_ (profile ^. UserUsername ==. val profilename)
    return follow
```
This one is just our standard join query.
While `insertFollows` a bit different.
```
insertFollows username profilename = do
  insertSelect $ from $ \(user, profile) -> do
    where_ (user ^. UserUsername ==. val username)
    where_ (profile ^. UserUsername ==. val profilename)
    return $ Follows <# (user ^. UserId) <&> (profile ^. UserId)
```
which translates into
```
insert into follows (follower_id, author_id)
select users.id, users2.id
from users, users as user2
where users.username = ? and users2.username = ?;
```
Pretty neat, huh?
But, this one is even neater.
```
deleteFollows username profilename = do
  delete $ from $ \follow -> do
    where_ $ exists $ from $ \(user, profile) -> do
      where_ (user ^. UserUsername ==. val username)
      where_ (profile ^. UserUsername ==. val profilename)
```

Current situation: [finished post and deleteUserFollowCoach](https://gitlab.com/ibnuda/real-world-conduit/commit/60e8186fa5112eb755b5458ba4de861a839372d2).

But, I should have [checked user's existence first](https://gitlab.com/ibnuda/real-world-conduit/commit/5927df2eddecd7d4ae34adc422497feb075f642f).

[Alright, trying to be smart doesn't work](https://gitlab.com/ibnuda/real-world-conduit/commit/ad4f1e1fba290855d4d37fa4481681c47fd1b757).

### Building `Coach`es for `articleApi`.
You know what? When I wrote my first try of this project's implementation, I
found out that there are three routes (or handlers) that can use the same query!
Although it will not look ["Haskal-ly" and much more "Lisp-y"](https://chrisdone.com/posts/haskell-lisp-philosophy-difference),
I think I can live with it.
You can read about that query in [note on join](2018-07-01-note-on-joins.html)
which I really suggest you to skim them.

The previous three routes are:

- `getArticlesCoach` which serve `/api/articles/`.
- `getArticlesFeedCoach` which serve `/api/articles/feed`.
- `getArticleSlugCoach` which serve `/api/articles/:slug`.

Let's build them.

```
getArticlesCoach authres mtag mauthor mfavorited mlimit moffset = do
  articles <-
    runDb $
    selectArticles
      (userUsername <$> authresToMaybe authres) -- move authresToMaybe to src/Util.hs
      False -- is it a feed?
      mtag
      mauthor
      mfavorited
      (fromMaybe 20 mlimit)
      (fromMaybe 0 moffset)
  return $ ResponseMultiArticle (map resultQueryToResponseArticle articles) (length articles)
```
It's pretty clear, I guess.
If you're wondering about `resultQueryToResponseArticle` function, this function
only "spread and extract" the value of the given parameter.

```
getArticlesFeedCoach (Authenticated user) mlimit moffset = do
  articles <-
    runDb $
    selectArticles
      (Just $ userUsername authres)
      True -- it is a feed!
      Nothing
      Nothing
      Nothing
      (fromMaybe 20 mlimit)
      (fromMaybe 0 moffset)
  return $ ResponseMultiArticle (map resultQueryToResponseArticle articles) (length articles)
getArticlesFeedCoach _ _ _ = throwError err401 {errBody = "only authenticated user."}
```
Same goes as this one.
Because `/api/articles/feed` is reserved for special route, it should be placed
right after `getArticlesCoach`.
The function itself doesn't differ much compared to the previous function.
It only sets `mtag`, `mauthor`, and `mfavorited` directly as `Nothing`.
The other distinction is, when an unauthorised request comes to it, it will
return a standard unauthorized error.
```
getArticleSlugCoach authres slug = do
  articles <-
    runDb $
    selectArticles
      (userUsername <$> authresToMaybe authres) -- could be a request from user.
      False
      Nothing
      Nothing
      Nothing
      1 -- slug is unique.
      0 -- only need a single article.
  case articles of
    []  -> throwError err404 {errBody = "no such article."}
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x
```
Well, I guess if you understand the previous function, you will understand this one.
Unfortunately, there's no native way to use named parameters without using an external
library like [named](https://hackage.haskell.org/package/named).

Oh yeah, don't forget to update `articlesApi` so it will look like this.
```
articlesApi authres =
  getArticlesCoach authres
  :<|> getArticlesFeed authres
  :<|> getArticleSlug authres
  :<|> panic ""
```

Current status: [finished getArticlesCoach, getArticlesFeedCoach, and getArticleSlug](https://gitlab.com/ibnuda/real-world-conduit/commit/0b2f050b2e4ac885f762427f2312c2079543e840).

#### Building `postArticleCreateCoach`.
Well, this handler receives a post request in form of `reqcreartic.json` in the readme file
of this project.
Servant's role here, like on the other handlers, is not much.
Basically, it just receives the data and pass it around in the system and then
returns the result.
Most of this operation happen in the database.
The steps are pretty much like the following:

1. Generates a random string just in case some users created title
   and description for the slug generation.
2. Inserts the article into the database.
3. Inserts the tags into the database.
4. Inserts the relationship between tags and the article into the database.
5. Returns the article in the required format.

So, let's do that.
First, we have to generate slug.
```
  ....
  randgen <- liftIO newStdGen
  let appedage = T.pack $ take 10 $ randomRs ('a', 'z') randgen
  ....

titleDescToSlug title desc appendage =
  smaller title <> "-" <> smaller desc <> "-" <> smaller appendage
  where
    smaller sentence =
      T.toLower (T.pack (subRegex (mkRegex "[^a-zA-Z0-9_.]") (T.unpack sentence) "-"))

```
Those two parts above are just functions which generate a text with 10 character
as the length and the other one is to remove special characters and make them
lower case.

Next part is inserting the article into the database.
The query itself is not complex, just a single `insert select`.
```
insertSelect username slug title desc body = do
  now <- liftIO getCurrentTime
  insertSelect $ from $ \user -> do
    where_ $ user ^. UserUsername ==. val username
    return $ Article <# slug <&> (user ^. UserId) <&> val title <&> val desc <&> val body <&> val now <&> nothing
```
As you can see, we create an `Article` object using the value which we got
from the result of that `where_` query in the middle.

After inserting the article, we should insert (or upsert?) the value of
the `tagList` from json into the database, if any.
```
upsertMaybeTags Nothing _ = return ()
upsertMaybeTags (Just tags) slug = do
  putMany $ map Tag tags
  insertSelect $ from $ \(article, tag) -> do
    where_ $ article ^. ArticleSlug ==. val slug
    where_ $ tag ^. TagName `in_` valList tags
    return $ Tagged <# (article ^. ArticleId) <&> (tag ^. TagId)
```
When there's no `tagList` in the json request, we should just calmly exist.
But when there's something there, we should update the existing tag, if any,
or just insert it.
Followed by inserting `Tagged` object from the parameters.

Finally, we are returning the apppropriate response after getting the previously
created article by querying the database based on the slug which was created earlier.

So, the final function will look like this.
```
postArticleCreateCoach (Authenticated user) req = do
  randgen <- liftIO newStdGen
  let appedage = T.pack $ take 10 $ randomRs ('a', 'z') randgen
      slug = titleDescToSlug reqtitle reqdesc appendage
  articles <- runDb $ do
    insertSelect article
    upsertMaybeTags reqtags
    selectArticles slug
  case articles of
    [] -> throwError err410 {errBody = "should be created but now is gone."}
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x
```
Please note that the parameters on the above functions almost completely omitted
for the sake of brevity.

Where are we? [finished creating postArticleCreateCoach](https://gitlab.com/ibnuda/real-world-conduit/commit/2bbc108f05e024bea4032adf1360f43b09bca5b9).

And [fixed a few things. non-consequentials i guess](https://gitlab.com/ibnuda/real-world-conduit/commit/265448043b24f0dd27211a696b4eccd606fc02ce).

#### Building `deleteArticleSlugCoach` and `putArticleSlugCoach`.
This part practically doesn't differ that much in term of business logic and its
inner working compared to updating users' information.
For example, when one wants to delete his article, server should:

1. Chcek the existence of the article and whether the sender of the request really
   the author of the article.
2. When that's the case, systme should just delete the article in question.
3. Returns `NoContent`.

While for updating the article, server does these:

1. Again, checking whether the request sender truly the author of the article in
   question.
2. Check the content of the request, when the request doesn't change anything,
   it should just throw error.
   I mean, why would one send an update request but doesn't want to change anything?
   That's just stupid.
3. Update it, of course.
4. Returns the updated article.

Now, building the handlers itself.

```
putArticleSlugCoach (Authenticated User {..}) slug (RequestUpdate req) = do
  authors <- runDb $ isArticleAuthor userUsername slug -- [1]
  when (null authors) $ throwError err401
  when (reqUpdateIsEmpty req) $ throwError err422 -- [2]
  articles <- runDb $ do
    updateArticle slug title description body -- [4]
    selectArticles (Just userUsername) False (Just slug) Nothing Nothing Nothing 1 0 -- [5]
  case articles of
    []  -> throwError err410
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x
putArticleSlugCoach _ _ _ = throwError err401

deleteArticleCoach (Authenticated user) slug = do
  authors <- runDb $ isArticleAuthor userUsername slug -- [1]
  when (null authors) $ throwError err401
  runDb $ deleteArticle slug -- [3]
  return NoContent
deleteArticleCoach _ _ = throwError err401
```
As you can see at the snippet above, the order of doing things here is pretty clear
and as defined above.
For example, for the marked snippet `[1]`, we are checking the wether the sender
of the request is the author themselves or not.
While the line marked `[2]`, denotes that we would not accept bullshit from users.
Of course, we are ruthless when it comes to deleting stuff.
As you can see at the line marked `[3]` above, we just simply delete things.
Look at the snippet for the `deleteArticle` function below.
```
import qualified Database.Persist.Class as PC
import qualified Database.Persist as P
deleteArticle slug = PC.deleteCascadeWhere [ArticleSlug P.==. slug]
```
And if you're wondering why do we update the article, it's pretty simple.
Because there are fields which could be sent as `Nothing`, we could simply reuse
`updateByMaybe` in the `Que.User` module.
Same goes for getting the updated article.
What we (presumably) have changed are title, description, and the body article.
That leaves the slug remains unchanged.
So we fetch it using the huge query function above (`selectArticles`).

### Building Comments Section
There are three sections here.

- The all the comments from an article.
- Post a comment to an article.
- Delete a comment from an article.

#### Get All Comments.

It's pretty simple actually.
```
getCommentsSlugCoach authres slug = do
  marticle <- runDb $ getBy $ UniqueSlug slug
  when (isNothing marticle) $ throwError err404
  comments <- runDb $ selectComments (userUsername <$> authresToMaybe authres) slug
  return $ ResponseMultiComment $ map resultQueryToResponseComment comments
```

#### Create a Comment.
Shitty code, tbh fam.
```
postCommentSlugCoach (Authenticated User {..}) slug (RequestComment (RequestComment body)) = do
  -- check article's existence.
  mcomment <- runDb $ insertComment userUsername slug body
  case mcomment of
    Nothing -> throwError err410
    Just co -> return $ ResponseComment $ resulQueryToResponseComment co
postCommentSlugCoach _ _ _ = throwError err401
```
#### Delete a Comment.
Even shittier, tbh fam.
```
deleteCommentSlugIdCoach (Authenticated User {..}) slug id = do
  -- check article's existence, author of the comment.
  -- throw error when fail.
  runDb $ deleteComment $ toSqlKey id
  return NoContent
deleteCommentSlugIdCoach _ _ _ = throwError err401
```

### Favoriting Things.
Pretty much the same as following.
The only difference is it talks to `favorited` tables.
