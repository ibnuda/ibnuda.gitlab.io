servant-auth, CORS, and Nginx
2018-07-24 05:33:33.007251724 UTC
Post

My old man asked to write a POS solution for his community project.
Something related to public well which will provide cheap water for the
poor or something like that.
So I decided to create an unnecessarily complex thing for him.

The solution will be consisted of 3 projects, a backend which is written in Haskell
, obviously, frontend in Elm (because I couldn't find a midsized Purescript project)
, and an android client for the dudes who check and input the water usages.
The backend part lives on [sumur-dalam](https://gitlab.com/ibnuda/sumur-dalam),
frontend on [timba](https://gitlab.com/ibnuda/timba), while the android
client will be written soon (tm).

The writing process for backend was really boring, to be quite honest (apart from
my usual habit to write anything twice).
But when I write the frontend using weird js things in Elm, I couldn't access
the REST part because of CORS , `OPTIONS` verb, and their friends.
I've tried a few solutions, that's for sure.
For example, I tried [`servant-options`](https://github.com/sordina/servant-options)
but got stuck at `GenerateList NoContent api (Foreign NoContent api)` class,
also tried this [servant's github issue on JWT Instance for `servant-foreign`](https://github.com/haskell-servant/servant-auth/issues/8)
and both of them made me realise how out of depth I am.

So, I decided to side step it and use nginx for this CORS issue.
I found out that, there are a lot people who have problems when it come to
CORS and used nginx to overcome it.
For example, this [stackoverflow question](https://stackoverflow.com/questions/45986631/how-to-enable-cors-in-nginx-proxy-server)
talks about CORS and it solved their issues.

Basically the following snippet will accept `OPTIONS` method and pass it to backend.
```
server {
  listen 80;
  server_name api.localhost;

  location / {
    proxy_redirect off;
    add_header 'Access-Control-Allow-Origin' '*';
    add_header 'Access-Control-Allow_Credentials' 'true';
    add_header 'Access-Control-Allow-Headers' 'Authorization,Accept,Origin,DNT,X-CustomHeader,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Content-Range,Range';
    add_header 'Access-Control-Allow-Methods' 'GET,POST,OPTIONS,PUT,DELETE,PATCH';

    if ($request_method = 'OPTIONS') {
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow_Credentials' 'true';
        add_header 'Access-Control-Allow-Headers' 'Authorization,Accept,Origin,DNT,X-CustomHeader,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Content-Range,Range';
        add_header 'Access-Control-Allow-Methods' 'GET,POST,OPTIONS,PUT,DELETE,PATCH';
        add_header 'Access-Control-Max-Age' 1728000;
        add_header 'Content-Type' 'text/plain charset=UTF-8';
        add_header 'Content-Length' 0;
        return 204;
    }

    proxy_redirect off;
    proxy_set_header host $host;
    proxy_set_header X-real-ip $remote_addr;
    proxy_set_header X-forward-for $proxy_add_x_forwarded_for;
    proxy_pass http://127.0.0.1:8080;
  }
}
```

Now, whenever I access the Elm frontend, I can use it just fine.
Every `OPTIONS` request is being handled by nginx.
