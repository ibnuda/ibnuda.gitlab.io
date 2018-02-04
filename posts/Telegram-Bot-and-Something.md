Telegram Bot and Something
2018-02-22 07:08:41.936771686 UTC
# Telegram Bot and Something (WIP)

One of my friend asked for a help.
Though this time from a different person and on different problem.
We are going to build a bot for Telegram instant messenger service which talks to database.
For the sake of giving an example, let's say that we will create a telegram bot which
records the income and expense.
Trust me, it's really hairy and ugly.

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
As for the reason, I've failed to compile [`telegram-api`](https://hackage.haskell.org/package/telegram-api)
in the server I'm deploying to. 

And then we will modify `package.yaml` to include the repo above as our dependency.

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

We will add a few dependencies on our `package.yaml`.

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

At the second glance, I just realized that we use so many language extensions.
As for the reasons, apart from `QuasiQuotes` and `TemplateHaskell`, I don't really
know and just accept that GHC will complain without them.
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

And don't forget to sprinkel some extensions so GHC can decide what kind of data
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
Also, we have to use an instance of `LoggingT` for this function (by append `runStderrLoggingT`)
to satisfy the demands from `withPostgresqlConn` so it can execute the query.

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

The same goes for many other functions we need. 
You can read it [here](https://gitlab.com/ibnuda/Telegram-Bot-Walkthrough/blob/feeb49d60a626047f4689072d29a0b3a06a4558f/src/ReadWrite.hs)
if you want to read the rest.

