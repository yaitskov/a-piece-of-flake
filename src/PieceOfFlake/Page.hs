{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Page where

import Data.Aeson ( encode )
import PieceOfFlake.Th ( includeFile )
import PieceOfFlake.Flake ( FlakeUrl, Flake, IpAdr(IpAdr) )
import PieceOfFlake.Index ( findFlakes )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(flakeIndex),
      trySubmitFlakeToRepo,
      popFlakeSubmition,
      addFetchedFlake )
import PieceOfFlake.Prelude hiding (Map)
import PieceOfFlake.Yesod
    ( clientAdrToDec4, getClientAdr, internalError, FavIcon(..) )
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
/flush.mp3 FlushSoundR GET
/submit-flake SubmitFlakeR POST
/fetch-new-flake-submitions FetchNewFlakeSubmitionsR POST
/find-flakes FindFlakesR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

getFaviconR :: Handler FavIcon
getFaviconR = pure $ FavIcon $(includeFile "assets/favicon.svg")

getGitHubR :: Handler FavIcon
getGitHubR = pure $ FavIcon $(includeFile "assets/github.svg")

getFlushSoundR :: Handler TypedContent
getFlushSoundR = pure . TypedContent "audio/mpeg" $ toContent $(includeFile "assets/flush.mp3")

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

            <div id=submition-form>
              <div class=BadFlake>
                <p class=date>
                  Now
                <p class=error>
                  Error about flake fetching or indexing
              <div class=FlakeIndexed>
                <p class=date>
                  Now
                <p>
                  <a href="#">Flake</a> has been indexed.
              <div class=FlakeFetched>
                <p class=date>
                  Now
                <p>
                  <a href="#">Flake</a> has been fetched.
              <div class=FlakeIsBeingFetched>
                <p class=date>
                  Now
                <p>
                  Flake fetching is in progress.
                  Press submit to refresh.
              <div class=SubmittedFlake>
                <p class=date>
                  Now
                <p>
                  <a href="#">Flake</a> has been submitted.
                  Press submit to refresh.
              <div class=submition-form>
                <form method=post onsubmit="return submitFlake(url.value)">
                  <fieldset>
                    <legend>Nix Flake Publication
                    <label>Flake URL
                      <input type=text id=flake-url name=url autofocus value="github:yaitskov/add-dependent-file"/>
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

           #submition-form .SubmittedFlake,
           #submition-form .FlakeIsBeingFetched,
           #submition-form .BadFlake,
           #submition-form .FlakeFetched,
           #submition-form .FlakeIndexed {
             display: none;
           }
           #submition-form.SubmittedFlake .SubmittedFlake,
           #submition-form.FlakeIsBeingFetched .FlakeIsBeingFetched,
           #submition-form.BadFlake .BadFlake,
           #submition-form.FlakeFetched .FlakeFetched,
           #submition-form.FlakeIndexed .FlakeIndexed {
             display: inherit;
           }
           |]


postSubmitFlakeR :: Handler Flake
postSubmitFlakeR = do
  requireCheckJsonBody >>= \fu -> do
    ip <- IpAdr . clientAdrToDec4 <$> getClientAdr
    Ypp { repo } <- getYesod
    trySubmitFlakeToRepo ip repo fu >>= \case
      Left e -> internalError e
      Right f -> pure f

postFetchNewFlakeSubmitionsR :: Handler (Maybe FlakeUrl)
postFetchNewFlakeSubmitionsR = do
  Ypp { repo } <- getYesod
  requireCheckJsonBody >>= \case
    Nothing -> do
      putStrLn "Just Fetch nex FlakeUrl"
      r <- popFlakeSubmition repo
      putBSLn $ toStrict $ encode r <> " :: Maybe Flake  <-> "  <> show r
      pure r
    Just fetchedFlake -> addFetchedFlake repo fetchedFlake


postFindFlakesR :: Handler [ FlakeUrl ]
postFindFlakesR = do
  Ypp { repo } <- getYesod
  requireCheckJsonBody >>= go repo
  where
    go repo = findFlakes repo.flakeIndex -- fsr .searchPattern
