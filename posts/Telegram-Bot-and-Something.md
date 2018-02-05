Telegram Bot and Something
2018-02-22 07:08:41.936771686 UTC
# Telegram Bot and Something (WIP)

One of my friend asked for a help.
Though this time from a different person and on different problem.
We are going to build a bot for Telegram instant messenger service which talks to database.
For the sake of giving an example, let's say that we will create a telegram bot which
records the income and expense.
Words of caution: trust me, it's really hairy and ugly.

Okay, let's start the mess.

### Preparation

Just type `stack new OurBot new-template` into your terminal and then modify `stack.yaml`.
Insert the following lines in it
```
# stack.yaml
packages:
- .
- location:
    git: https://github.com/fizruk/telegram-bot-simple
    commit: c1cc6bbba14ca79c897586e2de28433193c0b9fd
  extra-dep: true

```
Which means that we will use that git repo (on that specified commit) on our program.
As for the reason, I failed to compile [`telegram-api`](https://hackage.haskell.org/package/telegram-api)
in the server I'm deploying to. 

And then we will modify `package.yaml` to include the repo above as one of our dependencies.

```
# package.yaml
dependencies:
- base >= 4.7 && < 5
- telegram-bot-simple

```
Where `telegram-bot-simple` is the name of the library which we previously included
in our `stack.yaml`.

Let's build it for a giggle by issuing `stack build` at the project's root directory.
It should compile just fine, I guess.

Next, we will create our domain model.

### Database Thingy

We will add a few dependencies in our `package.yaml`.

- `persistent` for talking to database.
- `persistent-postgresql` for talking to postgresql, specifically.
- `persistent-template`, it eases our life a bit. Though prolong the compile time.
- `esqueleto` it's nice to join the tables in a <del>bar</del> database.
- `text`, I just like `Text`.
- `time`

Then, we will create a file named `Model.hs` in our `src` directory and include it
as one of our source files.

To simplify a bit, we will create two tables named `income` and `expense`.
The `income` table will have `source` (`Text`), `amount` (`Double`), and `when` (`UTCTime`) while `expense`
table will have `towhom` (`Text`), `amount` (`Double`), and `when` (`UTCTime`).

In order to accomplish that, we will edit `Model.hs` as the following
```
-- Model.hs
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Model where

import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Sql

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Income
      source Text
      amount Double
      when   UTCTime
      deriving Show Eq
    Expense
      towhom Text
      amount Double
      when   UTCTime
      deriving Show Eq
  |]

```

On a second glance, I just realized that we use so many language extensions.
As for the reasons, apart from `QuasiQuotes` and `TemplateHaskell`, I don't really
know and just accept that GHC will fail to compile without them.
But if you insist, you can look at the [docs](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)
about them.

Anyway, the snippet above, especially the `share` block, defines that we will
create tables name `income` and `expense`, which we have specified before the snippet.

And for the "migration plan", just a fancy way to say about creating and/or dropping
tables and/or database before the program talks to it, we will create a function
named `doMigration` in the same source file.
```
-- Model.hs
doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll

```
If you wonder where did `migrateAll` come from, that value comes from the
`share` block above.
You can see `"migrateAll"` there. That's the magic of `Template Haskell`.

#### Read Write

Let's start by adding `monad-logger`, `monad-control`, and `transformers` in our
`package.yaml` with the reason of satisfying a constraint of one of our functions.

We then create a source file named `ReadWrite.hs` on our `src` dir.
It will be really messy and we will have a lot of duplicated functions.
For example, a function like this:
```
-- ReadWrite.hs
insertIncome :: Text -> Double -> IO (Key Income)
insertIncome source amount = do
  now <- getCurrentTime
  runDb $ insert $ Income source amount now

insertExpense :: Text -> Double -> IO (Key Expense)
insertExpense towhom amount = do
  now <- getCurrentTime
  runDb $ insert $ Expense towhom amount now

```
To be honest, I don't really know how to reduce the duplication.
But that's another homework I have to solve for the next article, I guess.

Okay, let's import library for `ReadWrite.hs`.
```
-- ReadWrite.hs
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

import Data.Text
import Data.Time
import Data.Maybe

import Database.Persist.Postgresql (withPostgresqlConn)
import Database.Esqueleto

```
As you've seen, there are a lot of `Control.Monad` libraries.
Those libraries are used as the wrapper in our functions.
And the reason is the program we write communicates with `RealWorld(tm)` and
thus the data from/to our functions is safe.

And don't forget to sprinkle some extensions so GHC can decide what kind of data
we are using in the program.
```
-- ReadWrite.hs
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

```
As usual, I really suggest you to read through GHC's documentation to get the
explanations we need (or want) over what those extension do (or don't).

Finally, we come to the simplest part of the program, read write information
to the database.
```
-- ReadWrite.hs
--| For executing queries.
runDb ::
     (MonadIO m, MonadBaseControl IO m)
  => ReaderT SqlBackend (LoggingT m) a
  -> m a
runDb query = do
  -- Yep, you read that right.
  -- There's password and username hardcoded for db in here.
  let con = "host=localhost port=5432 user=ibnu dbname=bot password=jaran"
  runStderrLoggingT $ withPostgresqlConn con $ \backend -> runReaderT query backend

```
The function above takes a query, which is a read-only access to the database.
"Read-only" what I mean here is, in a sense, a paved road built by government.
You can't modify it, yet you can freely use it.
Also, we have to use an instance of `LoggingT` for this function (by appending `runStderrLoggingT`)
to satisfy the demands from `withPostgresqlConn` so it can execute the query properly.

After we wrote the executor, we will write the functions to read and write from/to database.
The snippet about `insert` above pretty much enough for our need of insert at the moment.
And for querying, we will create a few functions.

```
-- ReadWrite.hs
searchIncomeBySource ::
  (MonadBaseControl IO m, MonadIO m) => Text -> m [Income]
searchIncomeBySource source = do
  incomes <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. IncomeSource ==. val source)
      limit 10
      orderBy [desc (inc ^. IncomeWhen)]
      return inc
  return $ map entityVal incomes

```
The snippet above queries the database to select from table `income` which satisfy
the condition (`IncomeSource` equals to its argument) but only takes 10 rows and
orders it based on `when` column.

As I've complained above, about too many function duplications, we can create
a function for expense by replacing 4 token.
Just replace `Income` to `Expense` and presto! we have the function we want.

There's a really rough edge on this file, though.
```
totalExpense ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => ReaderT backend m Double
totalExpense = do
  res <- select $ from $ \exp -> return $ joinV $ sum_ (exp ^. ExpenseAmount)
  return $ head $ map (fromJust . unValue) res

balance :: IO Double
balance = do
  inc <- runDb totalIncome
  exp <- runDb totalExpense
  return $ inc - exp

```
The function `totalExpense` above disregards three possibilities:

- The table could be empty, thus the `res` value could be an empty list.
- Because `res` could be an empty list, the function `head` is not safe at all.
- And because `sum_`'s return value is `Maybe a`, the usage of `fromJust` is a really bad.

The same goes for many other functions we need. 
You can read it [here](https://gitlab.com/ibnuda/Telegram-Bot-Walkthrough/blob/feeb49d60a626047f4689072d29a0b3a06a4558f/src/ReadWrite.hs)
if you want to read the rest.

#### Types and Action

If you read the example from [telegram-bot-simple](https://github.com/fizruk/telegram-bot-simple),
you will know that the approach of this library is pretty similar to [miso](https://github.com/dmjio/miso)'s
or Elm's approach on state management.
And that means we have to define our `model` and `action` where every action on every state (or `model`)
maybe has a different effects on it.

So, let's define our `model` by open `Lib.hs` file on `src` dir.
```
-- Lib.hs
import Model
import ReadWrite

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

```
Just standard imports.
Nothing particularly interesting here.

```
-- Lib.hs
data ChatState
  = IncomeOrExpense Text Double
  | InsertingIncome Text
  | InsertingExpense Text
  | SearchingIncome
  | SearchingExpense
  | CheckingBalance
  | Other Text
  | EmptyContent
  deriving (Show, Eq)

```
We defined a sumtype named `ChatState` to model the state of the application.
As you have noted, there are a few types that's basically the same.
For example, `InsertingIncome` and `InsertingExpense` which is "only" a wrapped `Text`.
We do that because in order to insert an `Income` or `Expense`, based on the functions
that has defined in`ReadWrite.hs` takes two parameters, namely `towhom` (or `source`)
and `amount`, which `Text` and `Double` respectively, we only have to "hold" a `Text`
value before we take another `Double` value and then calling the `insert` function.

Same goes for the other functions, we only need to take `n - 1` arguments, if any, before
we call the corresponding query functions. 
 
```
-- Lib.hs
data ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

emptyChatModel = ChatModel EmptyContent

data Action
  = Empty
  | ActHelp
  | ActAddInc
  | ActAddExp
  | ActSearchIncome
  | ActSearchExpense
  | ActAddExp
  | ActAddMessageText Text
  | ActAddMessageDouble Double
  deriving (Show, Read)

```
On the snippet above, we defined `ChatState` as, well, the the state of the application
plus its content; or `model` in Elm's lingo.
We also defined `Action` that limits what could be done on this program.
For example, `ActHelp` is an action that will be used to tell the program to
show the user the "help messages."
And let the `Action` derives from `Show` and `Read` because we need them to be "show-able"
and "read-able".

Next, we will create an "app representation" of our session by adding the following lines.
```
-- Lib.hs
incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp
  { botInitialModel = emptyChatModel
  , botAction = flip updateToAction
  , botHandler = updateHandler
  , botJobs = []
  }

```
The defined structure of the `BotApp` specifies what could be done when given a specific `Update`.
Oh, forgot to tell, the message we (and bot) send is called `Update` in this package.

At the moment, `updateToAction` and `updateHandler` haven't been defined yet.
So take a moment to define those functions.

```
-- Lib.hs
updateToAction :: ChatModel -> Update -> Maybe Action
updateToAction _ = parseUpdate $
  ActHelp <$ command (pack "help") <|>
  ActBalance <$ command (pack "balance") <|>
  ActAddInc <$ command (pack "income") <|>
  ActAddExp <$ command (pack "expense") <|>
  ActSearchIncomes <$  command (pack "incomes") <|>
  ActSearchExpenses <$ command (pack "expense") <|>
  ActMessage <$ plainText <|>
  callbackQueryDataRead

```
This `updateAction` function basically takes any kind of chat models and parses any update it has given to
by read the message's text (`Update` has a lot of data in it).
If the message's text starts with a slash (`/`) then it will return the appropriate action.
But when the message's text does'nt start with a slash, it should regard the message's text as `ActMessage`.
