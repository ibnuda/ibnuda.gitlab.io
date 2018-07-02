Thinkster's RealWorld - Backend
2018-07-02 15:00:13.441855576 UTC
Post

(WIP)

### Background
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

### Prep
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

### Request and Response Types.
When you read RealWorld's [api spec.](https://github.com/gothinkster/realworld/tree/master/api),
you will see that there are a bunch of request and response schemas.
I, for one, am really forgetful person.
I can hardly remember which is which, so I prefer to define those schemas first
and let the compiler helps me recognise it.



##### Note
I use a lot of "feel" word when I write this because I'm pretty sure that when I do it,
I haven't had a decent experience and/or knoweldge to back up the thing I do.
