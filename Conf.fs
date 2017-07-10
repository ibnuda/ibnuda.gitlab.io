module Conf

open System

/// Site: Blog's url.
/// SiteTitle: The title of the blog.
/// Author: Author of the blog.
/// PagesPath: Path where the markdown files stored.
type Conf =
  { Site      : string
    SiteTitle : string
    Author    : string
    PublicPath: string
    PagesPath : string }
  /// Create a configuration.
  static member Create (site, title, author, pub, pages) =
    { Site      = site
      SiteTitle = title
      Author    = author
      PublicPath= pub
      PagesPath = pages }
  /// Default: mine.
  static member Default =
    Conf.Create ("https://ibnuda.gitlab.io", "Nothing Unusual", "IbnuDA", "public", "pages")

/// Create your own.
let conf = Conf.Default
