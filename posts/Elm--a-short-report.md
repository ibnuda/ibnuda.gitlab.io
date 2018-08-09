Elm: a short report
2018-08-01 12:46:23.721754309 UTC
Post

In the last post, I talked about my old man's project for public well or something
like that.
Now, based on my old man's statement, its web interface for administrative works
is done.
Bar a few minuscule improvements here and there.
And now, I guess I want to write something about my experience using Elm.

### Why?
Based on my track record, I tend to choose weird tech-stacks which are pretty alien
here in my land.
Furthermore, most of them related (dis)functional PLs which related to ML.
So, instead of using, say, React.js or Vue.js, I pick languages which compile
to JS and still related to ML.
In short, I don't want to get out of my comfort zone.
But why did I choose Elm instead of, say, Purescript, Fable, or GHCJS?

Last time I dabble in Purescript, I met a lot of hassles when it comes to library
and package management.
I mean, I've experienced a lot of confusion when I use them.
On one hand, the docs say that `pulp` will be deprecated soon.
While on the other hand, I can barely find a medium sized project which uses `psc-package`
for the build system.
Maybe it's not a big deal for you, but for me, I don't want to waste a lot of time
to hunt the docs.

And GHCJS (and Reflex, I guess), which is on the rise for Haskell folks, I couldn't
managed to compile the compiler.
And downloading the binary is not an option, for Arch decided to use weird configuration
for its libraries.
Something along `nopie` or what, I can't remember.

Fable? I couldn't even install F# compiler because of, again, my operating system.
Ahahaahahahaha. Frick...
I guess I should install [nixos](https://nixos.org/) soon.

What was left was Elm.
The last time when I'm playing around with [Elm(-ish)](https://github.com/elmish/elmish)
was almost a year ago.
But well, nothing particularly interesting because I couldn't remember what did
I write at the time.

### How?
I wrote a quick-and-dirty project for a playground for the current frontend.
With the helps from [elm-spa-example](https://github.com/rtfeldman/elm-spa-example)
and [elm-webpack-starter](https://github.com/simonh1000/elm-webpack-starter),
I can pretty much re-grasp Elm's "worldview" from a year ago when playing with
Elmish.

Basically, Elm considers that the "thing in our browser" as an "immutable record"
and when we want to change something, we should use "optics" (like [lens](http://hackage.haskell.org/package/lens)).
So, when we have a page on the browser and want to change something, input for
example, theoretically we destroy the whole page and re-create it with the (tiny)
modified page.
Though in practice, nobody is clumsy enough to actually do that and instead create
a "patch" in form of "messages and commands" to "update" the page.
Or something like that, I guess.

Look, I'm a stranger to web and its intricacies.
So I hope you forgive me with my weird analogies.

### Trials and Tribulations
Nothing much actually, apart from my naivety that writing UI and UX is simple.
The truth is, practically I spent more than half of the time to mix and match
CSS.
Sounds dumb, I know, but that's the fact.

For the actual program, pretty much a mechanical works.

1. I want to create a page. So,
2. I should create the route. Then,
3. I should write the page itself, including its "properties, getter, and setter". And,
4. I should also create its visual representations. Then,
5. I should introduce it into the entry point's "getter and setter." Finally,
6. I should watch it in the browser.

Pretty much that's it.
Nothing really interesting, really.

