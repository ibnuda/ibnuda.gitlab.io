Authorization in Freya using JWT
636353384514783965

Authorization in Freya (WIP)
============================

This blog post should be written after a few other guides completed. Or if you want and can stand really bad code, you can read at [Forum.Lock](https://gitlab.com/ibnuda/Forum/blob/master/src/Serv/Api.fs)

So, here's the codes:

```
  let routeContent =
    freyaHttpMachine {
      authorized Util.authorized
      methods [ GET ]
      handleOk (Represent.text <!> protReqHandler)
      handleUnauthorized (Represent.text <!> Freya.init "Unauthorized.")
    }
```
The important part is the `authorized Util.authorized` part. And `Util.authorized` itself is something like the following:
```
  module Util =
    open Forum.Lock
    let authorized =
      Request.header_ "authorization"
      |>  Freya.Optic.get
      >>= (Lock.authorize >> Freya.init)

```
and `Forum.Lock.authorize` is a function that compare the header (which is a JWT token) with the data on the database.
And I admit, that code is a spaghetti code and really smell bad.
