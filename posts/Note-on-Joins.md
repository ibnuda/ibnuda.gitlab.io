Note on Joins
2018-07-01 09:00:37.200926509 UTC
Post

Just in case I forgot things or something.

Given the following tables

```
users                articles
-----                --------
id (pk)              id (pk)
username (varchar)   slug (varchar)
                     body (varchar)
tags
----                 favorited
id (pk)              ---------
name (varchar)       id (pk)
                     user_id (fk, users.id)
tagged               article_id (fk, articles.id)
-----
id (pk)
article_id (fk, articles.id)
tag_id (fk, tags.id)

"articles"
 id |            slug             | author_id |           body
----|-----------------------------|-----------|--------------------------
  1 | a-title-and-its-description |         1 | this is a long axe thing.
  3 | title-desc                  |         1 | this is a long ask thing.
  4 | a-title-desc                |         1 | this is a long sos thing.
  5 | not-a-title-desc            |         1 | this is a long art thing.
  2 | something                   |         2 | nevermind.
  
"users"            "tagged"                    "tags"
 id | username      id | article_id | tag_id    id |   name
----|---------     ----|------------|--------  ----|----------
  1 | iaji           1 |          1 |      4     3 | nice
  2 | ibnu           2 |          3 |      1     1 | tag name
                     3 |          3 |      2     4 | nicer
                     4 |          4 |      1     5 | not nice
                     5 |          4 |      2     2 | eman gat
                     6 |          5 |      5
"favorited"
 id | user_id | article_id
----|---------|------------
  1 |       2 |          1


```

show article's id, slug, author name, tag names, favorite counts, and whether
have you favorited the article yet.

### 1. Getting Article's `id`, `slug`, and Author's `name`.

To get those fields, we can join `articles` and `users` tables.
And because there could be only one author for every article, we can user `inner join`.
```
select articles.id
     , articles.slug
     , users.username as authorname
from articles
inner join users on users.id = articles.author_id;
```
And that query will result something like this.
```
 id |            slug             | username 
----|-----------------------------|----------
  1 | a-title-and-its-description | iaji
  3 | title-desc                  | iaji
  4 | a-title-desc                | iaji
  5 | not-a-title-desc            | iaji
  2 | something                   | ibnu
```
Pretty clear, I pressume.
And the result of this query will be referred as `res1`.

### 2. Getting Article's `tag`.`name`s.

To get those field, we can join `res1` with `tagged` and `tags`.
Because a single article can have more than one tag and even has no tags, we will
use `outer join`.
And in this case, we will use `left outer join` because no matter what,
article is the most important piece of information in this query.
```
select articles.id
     , articles.slug
     , users.username as authorname
     , tags.name as tagnames
from articles
inner join users on users.id = articles.author_id
left outer join tagged on articles.id = tagged.article_id
left outer join tags on tags.id = tagged.tag_id
group by articles.id
       , users.username
       , tags.name;
```
Will result
```
 id |            slug             | authorname | tagnames 
----|-----------------------------|------------|----------
  5 | not-a-title-desc            | iaji       | not nice
  4 | a-title-desc                | iaji       | tag name
  4 | a-title-desc                | iaji       | eman gat
  1 | a-title-and-its-description | iaji       | nicer
  3 | title-desc                  | iaji       | tag name
  3 | title-desc                  | iaji       | eman gat
  2 | something                   | ibnu       |
```
You see that there are a few duplicate rows?
We can replace `tag.name as tagnames` with `array_agg(tags.name) as tagnames`
and remove `group by tags.name` so we can get the result like the following
```
 id |            slug             | authorname |        tagnames
----|-----------------------------|------------|-------------------------
  1 | a-title-and-its-description | iaji       | {nicer}
  2 | something                   | ibnu       | {NULL}
  3 | title-desc                  | iaji       | {"tag name","eman gat"}
  4 | a-title-desc                | iaji       | {"tag name","eman gat"}
  5 | not-a-title-desc            | iaji       | {"not nice"}
```
As you can see, we have "grouped" the result of `tagnames` in a single row.
The result of this query will be referred as `res2`.

### 3. Getting Article's `favorite` counts.
To get an `article`'s `favorite` counts, we can get that by `join`ing `res2`
with `favorited`
```
select articles.id
     , articles.slug
     , users.username as authorname
     , array_agg(tags.name) as tagnames
     , count(favorited.id) as favcounts
from articles
inner join users on users.id = articles.author_id
left outer join tagged on articles.id = tagged.article_id
left outer join tags on tags.id = tagged.tag_id
left outer join favorited on favorited.article_id = articles.id 
group by articles.id
       , users.username;
```
Which will result
```
 id |            slug             | authorname |        tagnames         | favcounts 
----|-----------------------------|------------|-------------------------|-----------
  1 | a-title-and-its-description | iaji       | {nicer}                 |         1
  2 | something                   | ibnu       | {NULL}                  |         0
  3 | title-desc                  | iaji       | {"tag name","eman gat"} |         0
  4 | a-title-desc                | iaji       | {"tag name","eman gat"} |         0
  5 | not-a-title-desc            | iaji       | {"not nice"}            |         0
```
We're almost done.
We only need the last thing, have we favorited it, yet?

### 4. Getting Our Favorite Status.
To get our status, we can use a sub query to check whether have we liked it or not.
Surely we can use subquery.
But, I don't know how does subquery affect our execution time.
Yeah, I know, I know about premature optimization is evil thingy.
```
select articles.id
     , articles.slug
     , users.username as authorname
     , array_agg(tags.name) as tagnames
     , count(favorited.id) as favcounts
     , (case when (exists (select *
                 from users as users2
                    , favorited as favd
                 where (users2.id = favd.user_id)
                   and (articles.id = favd.article_id)
                   and (users2.username = 'iaji')))
      then true
      else false
    end)
from articles
inner join users on users.id = articles.author_id
left outer join tagged on articles.id = tagged.article_id
left outer join tags on tags.id = tagged.tag_id
left outer join favorited on favorited.article_id = articles.id 
group by articles.id
       , users.username;
```
You see that `(case ...)` thingy?

### Generating it in Esqueleto.

```
getArticleTagNamesAndFavCounts username =
  select $
    from $ \(article
             `InnerJoin` author -- table users
             `LeftOuterJoin` tagged
             `LeftOuterJoin` tag
             `LeftOuterJoin` favd) -> do
      on (favd ^. FavoritedArticleId ==. art ^. ArticleId)
      on (tag ^. TagId ==. tagged ^. TaggedTagId)
      on (author ^. AuthorId ==. article ^. ArticleAuthorId)
      let favoriting =
        case_ [ when_
                  (exists $ from $ \(favorited, user) -> do
                    where_ (favorited ^. FavoritedArticleId ==. article ^. ArticleId)
                    where_ (favorited ^. FavoritedUserId ==. user ^. UserId)
                    where_ (user ^. UserUsername ==. val username))
                  then_ $ val True
              ]
              (else_ $ val Fase)
      groupBy (article ^. ArticleId)
      groupBy (author ^. UserUsername)
      return ( article ^. ArticleId
             , article ^. ArticleSlug
             , author ^. UserUsername
             -- This one will cause runtime error. Use sub_select instead.
             , arrayAgg (tag ^. TagName)
             , count (favd ^. FavoritedId))
```
