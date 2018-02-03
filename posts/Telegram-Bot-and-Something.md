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

Then, we will create a file named `Model.hs` in our `src` directory and include it
as one of our source files.

To simplify a bit, we will create two tables named `income` and `expense`.
The `income` table will have `source` (`Text`) and `amount` (`Double`) while `expense`
table will have `towhom` (`Text`), and `amount` (`Double`)

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
import Database.Persist.TH
import Database.Persist.Sql

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Income
      source Text
      amount Double
      deriving Show Eq
    Expense
      towhom Text
      amount Double
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
