# [Developing Web Apps with Haskell and Yesod, 2nd Edition][homepage], by Michael Snoyman, O'Reilly (2015)

[online reading][online_reading]

[homepage]: http://shop.oreilly.com/product/0636920035664.do
[online_reading]: http://www.yesodweb.com/book

## I. Basics

### 1. Introduction

Yesod and Hamlet (the default templating language) use `blaze-builder` for textual content generation.

### 2. Haskell

[Yesod quick start guide][quickstart], `cabal install alex happy`, `cabal update && cabal install yesod yesod-bin persistent-sqlite yesod-static`<br>
`{-# LANGUAGE MyExt #-}`, `-XMyExt`, `default-extensions` block in a cabal file, see [GHC Language Features][ghc_lang_features]<br>
`OverloadedStrings` and optional `ExtendedDefaultRules`<br>
[GHC/Type families][type_families]<br>
[Template Haskell][template_haskell] (TH) is essentially Haskell that generates a Haskell AST. Use `-ddump-splices` option to see a generated code. QuasiQuotes (QQ) are a minor extension of TH that let us embed arbitary content within our Haskell source files.<br>
[Stackage Server][stackage] > [Hoogle] search

[quickstart]: http://www.yesodweb.com/page/quickstart
[ghc_lang_features]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html
[type_families]: https://wiki.haskell.org/GHC/Type_families
[template_haskell]: https://wiki.haskell.org/Template_Haskell
[stackage]: https://www.stackage.org/
[hoogle]: https://www.haskell.org/hoogle/

### 3. Basics

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Yesod

data HelloWorld = HelloWorld

myYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello, World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

Every Yesod application has a found data type like `HelloWorld`. By the way, the word Yesod ( יסוד ) means *foundation* in Hebrew.<br>
Yesod is built on top of the Web Application Interface (WAI), allowing it to run on FastCGI, SCGI, Warp, or even as a desktop application using the WebKit library.<br>
`getHomeR = return $ object ["msg" .= "Hello, World"] -- aeson package for JSON data`<br>
scaffolded site - `yesod init`, `cabal install --only-dependencies`, `yesod devel` (rebuild and reload automatically)

### 4. Shakespearean Templates

[Shakespearean][shakespeare_package] family of template languages
* Hamlet (HTML)
* Julius (JavaScript)
* Cassius, Lucius (CSS, only syntactical difference)

[blaze-html][blazehtml_package] package provides the `Html` data type.<br>
...

[shakespeare_package]: www.stackage.org/package/shakespeare
[blazehtml_package]: https://hackage.haskell.org/package/blaze-html

### 5. Widgets

### 6. The Yesod Typeclass

### 7. Routing and Handlers

### 8. Forms

### 9. Sessions

### 10. Persistent

### 11. Deploying Your Web App

## II. Advanced

### 12. RESTful Content

### 13. Yesod's Monads

### 14. Authentication and Authorization

### 15. Scaffolding and the Site Template

### 16. Internationalization

### 17. Creating a Subsite

### 18. Understanding a Request

### 19. SQL Joins

### 20. Yesod for Haskellers

## III. Examples

### 21. Initializing Data in the Foundation Data Type

### 22. Blog: i18n, Authentication, Authorization, and Database

### 23. Wiki: Markdown, Chat Subsite, Event Source

### 24. JSON Web Service

### 25. Case Study: Sphinx-Based Search

### 26. Visitor Counter

### 27. Single-Process Pub/Sub

### 28. Environment Variables for Configuration

### 29. Route Attributes

## IV. Appendices

### A. monad-control

### B. Web Application Interface

### C. Settings Types

### D. http-conduit

### E. xml-conduit

