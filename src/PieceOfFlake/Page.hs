{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
module PieceOfFlake.Page where

import PieceOfFlake.Th ( includeFile )
import PieceOfFlake.Flake
import PieceOfFlake.Prelude hiding (Map)
import PieceOfFlake.Yesod
import Yesod.Core

-- import UnliftIO.Exception ( stringException, throwIO )

newtype Ypp
  = Ypp
    { repo :: FlakeRepo
    }

mkYesod "Ypp" [parseRoutes|
/app.js AppJsR GET
/robots.txt RobotsR GET
/sitemap.xml SiteMapR GET
/ HomeR GET
/favicon.svg FaviconR GET
/github.svg GitHubR GET
/submit-flake SubmitFlakeR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing



getFaviconR :: Handler FavIcon
getFaviconR = pure $ FavIcon $(includeFile "assets/favicon.svg")

getGitHubR :: Handler FavIcon
getGitHubR = pure $ FavIcon $(includeFile "assets/github.svg")

getSiteMapR :: Handler TypedContent
getSiteMapR = pure . TypedContent typeXml $ toContent $(includeFile "assets/sitemap.xml")

getRobotsR :: Handler TypedContent
getRobotsR = pure . TypedContent typePlain $ toContent $(includeFile "assets/robots.txt")

getAppJsR :: Handler TypedContent
getAppJsR = pure . TypedContent typeJavascript $ toContent $(includeFile "/home/dan/pro/haskell/my/a-piece-of-flake/a-piece-of-flake/assets/app.js")

getHomeR :: Handler Html
getHomeR =
 defaultLayout $ do
    setTitle "A Piece Of Flake"
    baseCss
    metaTags
    [whamlet|
            <h1>
              <a href="https://lficom.me">
                A Piece Of Flake

            <p>
              Nix Flake Repository

            <form method=post onsubmit="return submitFlake(url.value)">
              <fieldset>
                <legend>New Flake Publication
                <label>Flake URL
                  <input type=text name=url autofocus/>
                <div>
                  <button>publish</button>
            <center>
              <p>
                <a href="https://github.com/yaitskov/a-piece-of-flake">
                  <img class=github src=/github.svg />

            |]

metaTags :: WidgetFor Ypp ()
metaTags =
  toWidgetHead
    [hamlet|
           <meta charset="utf-8" />
           <meta name="viewport" content="width=device-width, initial-scale=1" />
           <meta name="author" content="Daniil Iaitskov" />
           <meta name="keywords" content="nix flake repository" />
           <meta name="description" content="Nix Flake repository" />
           <link rel="shortcut icon" href="favicon.svg" type="image/x-icon">
           <script src="/app.js"></script>
           |]

baseCss :: WidgetFor Ypp ()
baseCss =
  toWidgetHead
    [lucius|
           body {
               background: #7CED53;
               background: radial-gradient(circle, rgb(182 213 217) 0%, rgb(205 255 253) 80%, rgb(206 214 255) 100%);
               padding: 0px 4%;
               color: #171717;
           }
           img.github {
               width: 8vh;
               opacity: 0.8;
               padding-top: 5vh;
           }
           h1 a {
             font-size: x-large;
             text-decoration: none;
             color: #e64d4d;
           }
           pre {
               font-size: small;
           }
           .wrap {
               white-space: pre-wrap;
               text-indent: -2em;
               padding-left: 2em;
           }
           |]


postSubmitFlakeR :: HandlerFor Ypp Flake
postSubmitFlakeR = do
  requireCheckJsonBody >>= \fu -> do
    ip <- IpAdr . clientAdrToDec4 <$> getClientAdr
    Ypp { repo } <- getYesod
    trySubmitFlakeToRepo ip repo fu >>= \case
      Left e -> internalError e
      Right f -> pure f
