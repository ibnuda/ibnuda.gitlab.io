Static Blog Generator
2018-01-19 05:36:34.554073672 UTC
# Static Blog Generator

The previous blog engine, which was written in F#, was not really satisfying for me.
For example, I couldn't add a different kind of post without breaking things up.
Furthermore, it was really ugly; even for me who has very low standard of ugliness.

And then when I decided to write a new one, I realized that backward compatibility
was a real pain in the back.
So, I abandoned the backward support and wrote a new one from scratch and in haskall.

It shouldn't be either a program that has a lot of constraints and features like Hakyll
nor it would be a mess like my previous blog generator (you could laugh it
[here](https://gitlab.com/ibnuda/ibnuda.gitlab.io/tree/master/fs)).

#### Gathering the Requirements
What I wanted from a blog program was just it had to have the following features:

- Generates htmls from markdowns.
- Has two "kinds" of posts. For example, a normal post which displayed at the index
  and a special page, like "about".
- Generates index which based on the existing written posts.
- Can be hosted in any http server.
- Can create new post using command line.

#### Design, I Guess
Based on the previous wanted features, I decided to create a datatype like this.
```
data Content = Content
  { filename    :: Text
  , mdTitle     :: Text
  , mdDate      :: UTCTime
  , contentType :: Text
  , contentText :: Text
  } deriving (Show, Eq)

```
The snippet above represents the datatype which I thought could represents the entity
of the posts that have been written.
Whether it's the normal posts or the special posts.
As you can see, the `contentType` field represents that.
And if you wanted to ask "Why didn't you create a sum type for the content type", I was
too lazy to do that.

Other than the `Content` data wrapper, I also defined the data wrapper for configuration.
I heard that it is bad to hard-code strings or whatever into the program.
As a good sheep as I am, I created `Configuration` for the site.
```
data Configuration = Configuration
  { siteURL       :: Text
  , siteName      :: Text
  , author        :: Text
  , pathPages     :: Text
  , pathPosts     :: Text
  , pathGenerated :: Text
  } deriving (Show, Eq)

```
Though it just a couple of strings that used to define which directory the posts are stored
and boring stuff like that.

#### Markdown Read/Write
Basically, when I want to create a new post I just want to type "something new title" and
the program should handle the rest.
For example, the date of the writing being created, location of where should the markdown
be written, etc.

About that, I decided to write a function that takes two arguments, the type of the post
and the title.

So, I wrote the following function.
```
createMarkdownFile :: Text -> Text -> IO ()
createMarkdownFile cType cTitle = do
  now <- getCurrentTime
  let filename = mdFilenameFromTitle cTitle
      dir =
        case cType of
          "page" -> "pages"
          "post" -> "posts"
          _      -> "posts" :: Text
  writeContent $
    Content (filename <> ".md") cTitle now dir "# Write Here!!!\nPlease."

```
That function above "normalises" the title (so can be written without escape,
as seen in`mdFilenameFromTitle`) into the proper directory.

And when I want to "compile" the markdown files to html files, I should be able to
read the markdown files. Thus I write this function

```
write :: Text -> Text -> Text -> IO ()
write ctype fname content =
  writeFile (unpack ctype ++ "/" ++ unpack fname) content

```
Which is just a wrapper for `Data.Text.IO.writeFile` while append the name content type
and fname as the real filename of the written file.
While reading the file, I wanted it the same, I put two arguments (type and filename)
and will get a `Content` object.

```
readIndividualFile :: Text -> [Char] -> IO Content
readIndividualFile above nameOfFile = do
  content <- readFile fn
  case lines content of
    title:date:writing ->
      pure $
      Content (pack nameOfFile) title (TR.read $ unpack date) above $
      unlines writing
    _ -> pure emptyContent
  where
    fn = unpack above ++ "/" ++ nameOfFile

```
Considering the structure of the written markdown file,

```
This is the Title
2018-01-01 01:11:11.111111111 UTC --This is the date
# This is the Title

```
I had to skip two lines of the content of the file. Yeah, innefficient, I know.
Especially when I use the strict version of the `Data.Text.IO`, then load it
into memory, then `lines` it, and finally create a `Content` object (which also
`unlines` those `[Text]`).

#### Generating HTML
After the markdown files are succesfully read, the program only had to parse the
markdown content of the `Content` object into HTML and then write it into the
`pathGenerated`.
Though it's not that straightforward.

In order to generate HTML, I don't know, I feel stupid:

1. Read the all of the contents of the `pathPages` and `pathPosts` directory.
2. Generate the navbar.
3. Hold it in the memory.
4. While at it, gnerate the HTML contents from those two `paths` (with the navbar from number 2, obv.)
5. Generate index from a list of read `Content`s  from `pathPosts` directory.
6. Write the html files of the generated posts.
7. Write the index.
8. Write the generated pages from the step number 1.

I feel ashamed of myself. But well, at the moment, this program serves its purpose well,
I guess.
