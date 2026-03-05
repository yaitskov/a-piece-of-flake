{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Page where

import Data.Aeson ( encode )
import Data.Map.Strict (elems)
import PieceOfFlake.Th ( includeFile )
import PieceOfFlake.Flake
    ( FlakeUrl,
      Flake(..),
      IpAdr(IpAdr),
      MetaFlake(description, packages, rev),
      PackageInfo(broken, name, description, license, unfree) )
import PieceOfFlake.Index ( findFlakes )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(flakeIndex, flakes),
      trySubmitFlakeToRepo,
      popFlakeSubmition,
      addFetchedFlake )
import PieceOfFlake.Prelude hiding (Map, error, pi)
import PieceOfFlake.Yesod
    ( Ts(Ts),
      mp3Mime,
      bulmaLayout,
      clientAdrToDec4,
      getClientAdr,
      internalError )

import Text.Blaze.Internal ( MarkupM )
import Yesod.Core
import PieceOfFlake.CmdArgs ( StaticCacheSeconds, BaseUrl )
import StmContainers.Map ( lookup )


data Ypp
  = Ypp
    { repo :: FlakeRepo
    , staticCache :: Tagged StaticCacheSeconds Word32
    , baseUrl :: Tagged BaseUrl Text
    }

mkYesod "Ypp" [parseRoutes|
/app.js AppJsR GET
/style.css StyleR GET
/bulma.min.css BulmaR GET
/robots.txt RobotsR GET
/sitemap.xml SiteMapR GET
/ HomeR GET
/search SearchR GET
/flake/#FlakeUrl FlakeR GET
/publication PublicationR GET
/favicon.svg FaviconR GET
/github.svg GitHubR GET
/flush.mp3 FlushSoundR GET
/snow.mp3 SnowSoundR GET
/avalanche.mp3 AvalancheSoundR GET
/submit-flake SubmitFlakeR POST
/fetch-new-flake-submitions FetchNewFlakeSubmitionsR POST
/find-flakes FindFlakesR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

setCacheHeaderForStatic :: Handler ()
setCacheHeaderForStatic = do
  Ypp { staticCache } <- getYesod
  cacheSeconds . fromIntegral $ untag staticCache

sendStaticBs :: ToContent a => ByteString -> a -> Handler TypedContent
sendStaticBs mime c = do
  setCacheHeaderForStatic
  pure . TypedContent mime $ toContent c

getAppJsR, getFaviconR, getRobotsR, getGitHubR, getFlushSoundR :: Handler TypedContent
getSnowSoundR, getSiteMapR, getAvalancheSoundR, getStyleR, getBulmaR :: Handler TypedContent
getFaviconR = sendStaticBs typeSvg $(includeFile "assets/favicon.svg")
getGitHubR = sendStaticBs typeSvg $(includeFile "assets/github.svg")
getFlushSoundR = sendStaticBs mp3Mime $(includeFile "assets/flush.mp3")
getSnowSoundR = sendStaticBs mp3Mime $(includeFile "assets/snow.mp3")
getAvalancheSoundR = sendStaticBs mp3Mime $(includeFile "assets/avalanche.mp3")
getSiteMapR = sendStaticBs typeXml $(includeFile "assets/sitemap.xml")
getRobotsR = sendStaticBs typePlain $(includeFile "assets/robots.txt")
getAppJsR = sendStaticBs typeJavascript $(includeFile "assets/app.js")
getStyleR = sendStaticBs typeCss $(includeFile "assets/style.css")
getBulmaR = sendStaticBs typeCss $(includeFile "assets/bulma.min.css")

getSearchR :: Handler Html
getSearchR =
  bulmaLayout $ do
    setTitle "Nix Flake Search"
    metaTags
    navBar
    [whamlet|
            <section class="section pt-5">
              <h1 class="title is-4 mb-3">
                Nix Flake Search

              <form method=post onsubmit="return searchFlakesBy(pattern.value)">
                  <div class=field>
                    <div class=control>
                      <input type=text id=flake-pattern name=pattern
                             class=input
                             autofocus placeholder="vpn BSD3" value=""/>
                  <div class="field is-grouped">
                    <div class=control>
                      <button class="button is-black">Find</button>
                  <div class="field">
                    <div class="is-hidden">
                      <p class="help notification is-danger is-light" id=error-output>
                  <div class="field">
                    <div id=no-flakes-found class=is-hidden>
                      <div class="notification is-warning is-light">
                        No flakes were found. Try to simplify the search pattern.

              <div id=found-flakes class=is-hidden>
                <h2 class="title is-5 mb-1 pt-3">
                  Found flakes
                <table class=table>
                  <thead>
                    <tr>
                      <th>URL
                  <tbody id=search-results>
            |]

getHomeR :: Handler Html
getHomeR = getSearchR

getPublicationR :: Handler Html
getPublicationR =
 defaultLayout $ do
    setTitle "A Piece Of Flake"
    metaTags
    navBar
    [whamlet|
      <section class="section pt-4">
        <h1 class="title is-4 mb-3">
          Nix Flake Publication
        <div class=submition-form>
          <form method=post onsubmit="return submitFlake(url.value)">
            <div class=field>
              <label class=label>Flake URL
              <div class=control>
                <input type=text class=input name=url autofocus
                       placeholder="github:yaitskov/add-dependent-file"/>
            <div class="field is-grouped">
              <div class=control>
                <button class="button is-black">Publish</button>
           <div class=field>
             <div id=bad-url class=is-hidden>
               <div class="notification is-danger">
                 Url does not fit the pattern: <b>github:owner/project
            <div class=field>
              <div id=sumbitted-notification class=is-hidden>
                <div class="notification is-success">
                  Flake has been submitted.
                  Check flake status <a id=flake-link href="#">here
            <div class=field>
              <div id=error-output-hid class=is-hidden>
                <pre id=error-output class="notification is-danger">
            |]

-- getAboutR :: Handler Html
-- getStatsR :: Handler Html
-- getStatsR = do
--                 <center>
--               <p>
--                 <a href="https://github.com/yaitskov/a-piece-of-flake">
--                   <img class=github src=/github.svg />

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
           <link rel=stylesheet href=/bulma.min.css>
           <link rel=stylesheet href=/style.css>
           <script src="/app.js"></script>
           |]

getFlakeR :: FlakeUrl -> Handler Html
getFlakeR fu = do
  Ypp { repo } <- getYesod
  fw <- atomically (lookup fu repo.flakes) <&> \case
    Nothing -> flakeNotFound
    Just f -> flakeToWidget f

  bulmaLayout $ do
    setTitle "Nix Flake Search"
    metaTags
    navBar
    [whamlet|
          <section class="section pt-4">
            <h1 class="title is-4 mb-3">
              Flake
            ^{fw}
            |]
  where
    flakeNotFound =
      [hamlet|
             <div class="notification is-danger">
               Flake #{fu} is not found
             |]
    flakeToWidget f =
      case f of
        SubmittedFlake { flakeUrl, submittedAt, submittedFrom } ->
          [hamlet|
            <table class=table>
              <tbody>
                <tr>
                  <td>
                    Url
                  <td>
                    #{flakeUrl}
                <tr>
                  <td>
                    Status
                  <td>
                    <details>
                      <summary class="notification is-info p-1">
                        Submitted
                      The request for flake publication is accepted.
                      Request status life-cycle:
                      <ul>
                        <li>Submitted
                        <li>On Fetcher
                        <li>Fetched
                        <li>Indexed - flake is discoverable by search request
                <tr>
                  <td>
                    Timestamp
                  <td>
                    #{Ts submittedAt}
                <tr>
                  <td>
                    From
                  <td>
                    #{submittedFrom}
                |]
        FlakeIsBeingFetched { flakeUrl, submitionFetchedAt, fetcherId } ->
          [hamlet|
            <table class=table>
              <tbody>
                <tr>
                  <td>
                    Url
                  <td>
                    #{flakeUrl}
                <tr>
                  <td>
                    Status
                  <td>
                    <details>
                      <summary class="notification is-info p-1">
                        On Fetcher
                      Fetcher (Worker) picked up the request
                      for downloadand evaluation of the flake.
                <tr>
                  <td>
                    Timestamp
                  <td>
                    #{Ts submitionFetchedAt}
                <tr>
                  <td>
                    Fetcher
                  <td>
                    #{fetcherId}
                |]
        BadFlake { flakeUrl, fetcherRespondedAt, error } ->
          [hamlet|
            <table class=table>
              <tbody>
                <tr>
                  <td>
                    Url
                  <td>
                    #{flakeUrl}
                <tr>
                  <td>
                    Status
                  <td>
                    <details>
                      <summary class="notification is-danger p-1">
                        Rejected
                      Fetcher failed to obtain flake or parse it.
                      Check flake url and error message.
                      Fix the issue and resubmit the flake for publication.
                <tr>
                  <td>
                    Timestamp
                  <td>
                    #{Ts fetcherRespondedAt}
                <tr>
                  <td>
                    Error
                  <td class="notification is-danger">
                    <pre>#{error}
                |]
        FlakeFetched { flakeUrl, uploadedAt, meta} ->
          let
            ps :: [PackageInfo] = concatMap elems (elems meta.packages)
            pkgWgs p = mapM_ (\ff -> ff p) (packageInfoToWidget <$> ps) in
             [hamlet|
               <table class=table>
                 <tbody>
                   <tr>
                     <td>
                       Url
                     <td>
                       #{flakeUrl}
                   <tr>
                     <td>
                       Status
                     <td>
                       <details>
                         <summary class="notification is-warning p-1">
                           Fetched
                         Information about the flake is fetched by fetcher
                         and uploaded into database for consequent indexing.
                   <tr>
                     <td>
                       Timestamp
                     <td>
                       #{Ts uploadedAt}
               <h6 class="title is-6 mb-3">
                 Packages
               ^{pkgWgs}
                   |]
        FlakeIndexed  { flakeUrl, indexedAt, meta} ->
          let
            ps :: [PackageInfo] = concatMap elems (elems meta.packages)
            pkgWgs p = mapM_ (\ff -> ff p) (packageInfoToWidget  <$> ps) in
             [hamlet|
               <table class=table>
                 <tbody>
                   <tr>
                     <td>
                       Url
                     <td>
                       #{flakeUrl}
                   <tr>
                     <td>
                       Status
                     <td>
                       <details>
                         <summary class="notification is-success p-1">
                           Indexed
                         The flake has been indexed and now is discoverable by search
                   <tr>
                     <td>
                       Timestamp
                     <td>
                       #{Ts indexedAt}
                   <tr>
                     <td>
                       Revision
                     <td>
                       #{meta.rev}
                   <tr>
                     <td>
                       Description
                     <td>
                       #{fromMaybe "n/a" meta.description}

               <h4 class="title is-4 mb-3">
                 Packages
               ^{pkgWgs}
                   |]

packageInfoToWidget :: PackageInfo -> p -> MarkupM ()
packageInfoToWidget (pi :: PackageInfo) =
  [hamlet|
     <details>
      <summary>
        Package #{pi.name}
      <table class="table">
        <tr>
          <td>Description
          <td>#{pi.description}
        <tr>
          <td>License
          <td>#{pi.license}
        <tr>
          <td>Unfree
          <td>#{pi.unfree}
        <tr>
          <td>Broken
          <td>#{pi.broken}
         |]

navBar :: WidgetFor Ypp ()
navBar = do
  Ypp { baseUrl } <- getYesod
  toWidgetBody [hamlet|
         <nav class=navbar role=navigation aria-label="main navigation">
           <div class=navbar-brand>
             <a class="navbar-item has-text-weight-bold"
                href=#{untag baseUrl}>
                <img src="/favicon.svg">
                A Piece of Flake
             <a role=button aria-label=menu aria-expanded=false
                data-target=navbar-menu id=navbar-burger class=navbar-burger>
               <span aria-hidden="true">
               <span aria-hidden="true">
               <span aria-hidden="true">
               <span aria-hidden="true">
           <div id=navbar-menu class=navbar-menu>
             <div class="navbar-start">
               <a class="navbar-item" href=@{PublicationR}>
                 Publish
               <a class="navbar-item" href=@{SearchR}>
                 Search
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
    go repo = findFlakes repo.flakes repo.flakeIndex -- fsr .searchPattern
