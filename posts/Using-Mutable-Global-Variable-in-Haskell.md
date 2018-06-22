Using Mutable Global Variable in Haskell
2018-06-22 05:30:46.943694944 UTC
Post
Or perhaps you can call it a black magic.

### Backstory
Last night, Oom Boy (that's his real name) asked me whether I have ever used global
variable in my programs.
He said that he needs to keep a single integer (for goal count or something?) 
which could be updated in a long running process.
Both of us agreed that using an external value holder, sqlite for example, has too
much of an overhead.
Surely I sent him a link from [Haskell wiki](https://wiki.haskell.org/Top_level_mutable_state#A_variant:_use_.22deriving.22_clauses)
and wrote a simple [POC](https://gitlab.com/ibnuda/SaltedEgg/tree/2bdf47f73a685f70a19929e1f1cb35e654f0bc2c) within a web server
context.
Yeah, not only I'm just a single trick pony, I also too lazy to write programs
that runs for a long time and has some interactivity in it.

#### Confession
Truth to be told, I actually tried to implement it for one of my `$WORK`'s
project but decided not to because I was scared, man.
I was scared because I don't know what actually happens under the hood
of `unsafePerformIO` and that scary looking derivation.

### Looking for a Viable Solution
Back to my friend's question and the program above, I'm pretty sure that it's not
a wise decision to give a solution which I don't understand myself.
So, I decided to look for existing libraries about mutable global state (hereon MGS)
and found `global-variables` and `safe-globals` from a quick search.
After poking those two repo's around for a bit, I've found out that I couldn't
understand what `global-variables` doing while I "get" what `safe-globals` does
to create MGS.

Basically, it creates a top level `IORef` (or `MVar`, or whatever it exports)
declarations which can accessed anywhere using the magic of `TemplateHaskell`.
For example:
```
declareIORef "ourreference" [t|Int|] [|69|]
```
when we put that in our source file, and as long as we can access it, we can use
`ourreference :: IORef Int` everywhere.
Why, you ask?
Because we have declared that we have an `IORef` with the name of `ourreference`
which has `Int` as its content with the default value of `69`.
Ain't that nice?
It's nice.

#### Execution
Unfortunately, when I tried to use that library in my program above, I get an error
about compilation failure or something like that.
So, I forked that thing and tried to compile it then found out that this library
has been abandonen by the author.

I took the easiest path, I forked the library and pressed my keyboard furiously
while hoping it compiles.
Fortunately, God didn't leave me, it [compiles](https://github.com/ibnuda/safe-globals/commit/1aad152b573aa39a1fc5c2acf1de2f036947f5b3)!

The next step was trying put that to that fork to my program above.
From this:
```
-- A few classes, newtypes, and instances.

somethingInt :: Int -> IO Int
somethingInt i = do
  UniqueRef r <- runOnceIO
  n <- readIORef r
  writeIORef r (n + i)
  return (n + i)

-- And I don't know where to set the default value
-- perhaps this?
onceUniqueRef = unsafePerformIO $ newMVar Nothing
```
to [this](https://gitlab.com/ibnuda/SaltedEgg/commit/70076265165dbb3063daf3983ded1303e5c97f9b):
```
-- Here I declared `egg` as IORef thingy.
declareIORef "egg" [t|Int|] [|0|]

-- renamed from `somethingInt` to `joinOurEggs`.
joinOurEggs :: Int -> IO Int
joinOurEggs youreggs = do
  myeggs <- readIORef egg
  let oureggs = myeggs + youreggs
  writeIORef egg oureggs
  return oureggs
```
That's it!
It's pretty clearer, right?

#### Word of Cautions
This section is intended to myself to never ever forget about data consistency,
availability, and stuff like that.
ACID, man! Not that Acid, though.

### Lesson Learned
It's hard when I face a dilemma over API deprecation and introducing a
new dependency (or two, three, six, etc).
This time, I decided to bite the bullet and added `SafeSemaphore` to replace
`SampleVar` from `base` and introduced `safe-globals` to my program above.
Well, can't cry over spilled milk, right?
