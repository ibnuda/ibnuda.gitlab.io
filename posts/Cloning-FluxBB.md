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

Now, let's push it to our repo.
