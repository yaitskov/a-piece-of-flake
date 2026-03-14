{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultilineStrings #-}
module PieceOfFlake.Page where

import Data.Aeson ( encode )
import Data.Map.Strict (elems)
import PieceOfFlake.CmdArgs
    ( WsCmdArgs(logLevel), FetcherSecret, BaseUrl, StaticCacheSeconds )
import PieceOfFlake.Flake
    ( FlakeUrl,
      Flake(..),
      linkUrl,
      IpAdr(IpAdr),
      MetaFlake(description, packages, rev),
      PackageInfo(broken, name, description, license, unfree) )
import PieceOfFlake.Flake.Repo
    ( FetcherReq(FetcherReq),
      FlakeRepo(wsArgs, fetcherSecret, flakes, fetcherQueueLen,
                flakeIndex, repoStats),
      trySubmitFlakeToRepo,
      popFlakeSubmition,
      addFetchedFlake,
      validateRawFlakeUrl )
import PieceOfFlake.Index ( findFlakes, listQueryCache, FlakeIndex (searchRequestCounter, indexerQueueLen) )
import PieceOfFlake.Prelude hiding (Map, error, pi, Handler)
import PieceOfFlake.Stats ( greadTraVar, renderRepoStats )
import PieceOfFlake.Th ( includeFile )
import PieceOfFlake.Yesod
    ( ContentEncoding(Br, Gzip),
      Mime(Mime),
      Ts(Ts),
      staticFile,
      sendStaticBs,
      mp3Mime,
      bulmaLayout,
      clientAdrToDec4,
      getClientAdr )
import StmContainers.Map ( lookup )
import Text.Blaze.Internal ( MarkupM )
import Yesod.Core
    ( Yesod(defaultLayout, shouldLogIO, maximumContentLength,
            makeSessionBackend, approot),
      RenderRoute(renderRoute),
      TypedContent,
      HandlerFor,
      Html,
      Approot(ApprootMaster),
      WidgetFor,
      ToWidgetHead(toWidgetHead),
      ToWidgetBody(toWidgetBody),
      mkYesod,
      parseRoutes,
      typeSvg,
      typeJavascript,
      typeCss,
      typeXml,
      typePlain,
      getYesod,
      setTitle,
      whamlet,
      hamlet,
      requireCheckJsonBody,
      invalidArgs,
      permissionDenied )



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
/stats StatsR GET
/about AboutR GET
/flake/#FlakeUrl FlakeR GET
/publication PublicationR GET
/favicon.svg FaviconR GET
/flake.svg FlakeSvgR GET
/github.svg GitHubR GET
/flush.mp3 FlushSoundR GET
/snow.mp3 SnowSoundR GET
/avalanche.mp3 AvalancheSoundR GET
/submit-flake SubmitFlakeR POST
/fetch-new-flake-submitions FetchNewFlakeSubmitionsR POST
/find-flakes FindFlakesR POST
|]

instance Yesod Ypp where
  approot = ApprootMaster $ untag . baseUrl
  makeSessionBackend _ = pure Nothing
  maximumContentLength _ = pure . \case
    Nothing -> 1
    Just HomeR -> 1
    Just SubmitFlakeR -> 200
    Just FetchNewFlakeSubmitionsR -> 100000
    Just _ -> 500

  shouldLogIO (Ypp {repo}) _ l =
    pure $ l >= repo.wsArgs.logLevel


getAppJsR, getFaviconR, getRobotsR, getGitHubR, getFlushSoundR :: Handler TypedContent
getSnowSoundR, getSiteMapR, getAvalancheSoundR, getStyleR, getBulmaR, getFlakeSvgR :: Handler TypedContent
getFlakeSvgR =
  staticFile (Mime typeSvg) $(includeFile "assets/flake.svg") $ fromList
    [ (Gzip, $(includeFile "assets/flake.svg.gz"))
    , (Br, $(includeFile "assets/flake.svg.br"))
    ]
getFaviconR =
  staticFile (Mime typeSvg) $(includeFile "assets/favicon.svg") $ fromList
    [ (Gzip, $(includeFile "assets/favicon.svg.gz"))
    , (Br, $(includeFile "assets/favicon.svg.br"))
    ]
getGitHubR =
  staticFile (Mime typeSvg) $(includeFile "assets/github.svg") $ fromList
    [ (Gzip, $(includeFile "assets/github.svg.gz"))
    , (Br, $(includeFile "assets/github.svg.br"))
    ]
getAppJsR =
  staticFile (Mime typeJavascript) $(includeFile "assets/app.js") $ fromList
    [ (Gzip, $(includeFile "assets/app.js.gz"))
    , (Br, $(includeFile "assets/app.js.br"))
    ]
getStyleR =
  staticFile (Mime typeCss) $(includeFile "assets/style.css") $ fromList
    [ (Gzip, $(includeFile "assets/style.css.gz"))
    , (Br, $(includeFile "assets/style.css.br"))
    ]
getBulmaR =
  staticFile (Mime typeCss) $(includeFile "assets/bulma.min.css") $ fromList
    [ (Gzip, $(includeFile "assets/bulma.min.css.gz"))
    , (Br, $(includeFile "assets/bulma.min.css.br"))
    ]
getFlushSoundR = sendStaticBs mp3Mime $(includeFile "assets/flush.mp3")
getSnowSoundR = sendStaticBs mp3Mime $(includeFile "assets/snow.mp3")
getAvalancheSoundR = sendStaticBs mp3Mime $(includeFile "assets/avalanche.mp3")
getSiteMapR = sendStaticBs (Mime typeXml) $(includeFile "assets/sitemap.xml")
getRobotsR = sendStaticBs (Mime typePlain) $(includeFile "assets/robots.txt")

instance ClockMonad (HandlerFor Ypp) where
  getCurrentTime = liftIO getCurrentTime
  getTimeAfter x =  liftIO $ getTimeAfter x

getStatsR :: Handler Html
getStatsR = do
  Ypp { repo } <- getYesod
  queries <- listQueryCache repo.flakeIndex
  fetchQueueLen <- Tagged @"fetch" <$> readTVarIO repo.fetcherQueueLen
  idxQueueLen <- readTVarIO repo.flakeIndex.indexerQueueLen
  searchReq <- readTVarIO repo.flakeIndex.searchRequestCounter
  rs <- greadTraVar repo.repoStats
  bulmaLayout $ do
    setTitle "Stats"
    metaTags
    navBar
    [whamlet|
            <section class="section pt-5">
              ^{renderRepoStats searchReq idxQueueLen fetchQueueLen rs}
              <h2 class="title is-4 mb-3">
                Popular Queries
              $if null queries
                <div class="notification is-warning">
                   No queries
              $else
                <div class=content>
                  <ul>
                    $forall q <- queries
                      <li>
                        #{q}
            |]

getSearchR :: Handler Html
getSearchR =
  bulmaLayout $ do
    setTitle "Search - A Piece of Flake"
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
                    <div>
                      <p class="help notification is-info is-light" id=search-hint>
                        Besides text from flake metadata the text index contains keywords:
                        nixosModules, unfree, free, broken, and unbroken.
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
    setTitle "Publication - A Piece Of Flake"
    metaTags
    navBar
    [whamlet|
      <div id=aniflakes>
        <img class="ani fall1" src="flake.svg">
        <img class="ani fall2" src="flake.svg">
        <img class="ani fall3" src="flake.svg">
        <img class="ani fall4" src="flake.svg">

      <img class="ani ani-fall" src="/flake.svg">
      <section class="section pt-4">
        <h1 class="title is-4 mb-3">
          Nix Flake Publication
        <div class=submition-form>
          <form method=post onsubmit="return submitFlake(url.value)">
            <div class=field>
              <label class=label>Flake URL
              <div class=control>
                <input type=text class=input name=url autofocus
                       placeholder="github:owner/repository"/>
            <div class="field is-grouped">
              <div class=control>
                <button class="button is-black">Publish</button>
           <div class=field>
             <div id=bad-url class=is-hidden>
               <div class="notification is-danger content">
                 Url does not fit the pattern:
                 <ul>
                   <li>
                     <p>
                       github:owner/project
                   <li>
                     <p>
                       https://github.com/owner/project
            <div class=field>
              <div id=sumbitted-notification class=is-hidden>
                <div class="notification is-success">
                  Flake has been submitted.
                  Check flake status <a id=flake-link href="#">here
            <div class=field>
              <div id=error-output-hid class=is-hidden>
                <pre id=error-output class="notification is-danger error">
            |]

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
  setTitle "About - A Piece Of Flake"
  metaTags
  navBar
  [whamlet| $newline always
      <section class="section pt-4">
        <h1 class="title is-4 mb-3">
           About
        <div class="content is-size-5 has-text-justified">
          <p>
             The main idea behind this project is to provide a quick and simple
             interface for publishing Nix flakes.

          <p>
             Flakes have become relatively mature and address the central
             repository issue. However, the Nixpkgs repository on GitHub still has
             more that 5k open issues and a comparable number of pull requests, and
             continues to receive many commits every day.  Getting a pull request
             for a new tool merged into Nixpkgs can be difficult - the Nixpkgs
             README explicitly discourages people from submitting their "pet"
             projects.

          <p>
             The Nixpkgs repository is huge. It contains more than 120k packages, but
             the majority of them are not native to Nix. For example, about 10%
             are Haskell packages imported. Therefore, this large number cannot be
             used as a reliable measure of how well the publishing process is
             developed in Nix. For instance, the PyPy repository alone currently
             contains almost 900k packages.

          <p>
             It is also important to note Python is the most popular
             general-purpose programming language, and its publishing process was
             designed by programmers for programmers. Yet there is no pull-request
             step in the workflow. The interface is essentially "upload and
             forget", which has a significant positive impact on the conversion
             funnel of Python packages.

          <p>
             Flakes are easy to install, but the publishing workflow is not yet polished
             enough. The current approach to distributing flakes appears to have
             inherinted many characteristics of the Nixpkgs workflow.

          <p>
             For Nixpkgs, this was the natural way of development, because all
             derivations form a large and coupled Nix expression split across many
             files within a single Git repository.
          <p>
            <center>
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
    setTitle $ show fu <> " - A Piece of Flake"
    metaTags
    navBar
    [whamlet|
          <section class="section pt-4">
            <h1 class="title is-4 mb-3">
              Flake
            ^{fw}
            |]
  where
    flakeStatusLifeCycle =
      [hamlet|
             <p>Request status life-cycle:
             <ul>
               <li>Submitted
               <li>On Fetcher
               <li>Fetched
               <li>Indexed - flake is discoverable by search request
             <p>
               <a href=@{StatsR}>
                 Average processing time
             |]
    flakeNotIndexed =
      [hamlet|<div id=flake-is-not-indexed>
             |]
    flakeNotFound =
      [hamlet|
             <div class="notification is-danger">
               Flake #{fu} is not found
             |]
    flakeToWidget f =
      case f of
        SubmittedFlake { flakeUrl, submittedAt, submittedFrom } ->
          [hamlet|
            ^{flakeNotIndexed}
            <table class=table>
              <tbody class=content>
                <tr>
                  <td>
                    Link
                  <td>
                    <a href="#{linkUrl flakeUrl}">
                      #{flakeUrl}
                <tr>
                  <td>
                    Status
                  <td>
                    <details>
                      <summary class="notification is-info p-1">
                        Submitted
                      <p>The request for flake publication is accepted.
                      ^{flakeStatusLifeCycle}
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
            ^{flakeNotIndexed}
            <table class=table>
              <tbody class=content>
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
                      <p>
                        Fetcher (Worker) picked up the request
                        for downloadand evaluation of the flake.
                      ^{flakeStatusLifeCycle}
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
              <tbody class=content>
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
                      <p>Fetcher failed to obtain flake or parse it.
                      <p>Check flake url and error message.
                      <p>Fix the issue and resubmit the flake for publication.
                <tr>
                  <td>
                    Timestamp
                  <td>
                    #{Ts fetcherRespondedAt}
                <tr>
                  <td>
                    Error
                  <td class="notification is-danger">
                    <pre class=error>#{error}
                |]
        FlakeFetched { flakeUrl, uploadedAt, meta} ->
          let
            ps :: [PackageInfo] = concatMap elems (elems meta.packages)
          in
             [hamlet|
               ^{flakeNotIndexed}
               <table class=table>
                 <tbody class=content>
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
                         <p>Information about the flake is fetched by fetcher
                            and uploaded into database for consequent indexing.
                   <tr>
                     <td>
                       Timestamp
                     <td>
                       #{Ts uploadedAt}
               <h6 class="title is-6 mb-3">
                 Packages
               $if null ps
                 <div class="notification is-warning">
                   No packages
               $else
                 $forall p <- ps
                   ^{packageInfoToWidget p}
                   |]
        FlakeIndexed  { flakeUrl, indexedAt, meta} ->
          let
            ps :: [PackageInfo] = concatMap elems (elems meta.packages)
          in
             [hamlet|
               <table class=table>
                 <tbody class=content>
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
                         <p>
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
               $if null ps
                 <div class="notification is-warning">
                   No packages
               $else
                 $forall p <- ps
                   ^{packageInfoToWidget p}
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
               <a class="navbar-item" href=@{StatsR}>
                 Stats
               <a class="navbar-item" href=@{AboutR}>
                 About
         |]

postSubmitFlakeR :: Handler Flake
postSubmitFlakeR = do
  requireCheckJsonBody >>= \rfu ->
    case validateRawFlakeUrl rfu of
      Nothing -> invalidArgs ["flake url formatting is wrong"]
      Just fu -> do
        ip <- IpAdr . clientAdrToDec4 <$> getClientAdr
        Ypp { repo } <- getYesod
        trySubmitFlakeToRepo ip repo fu >>= \case
          Left e -> invalidArgs [e]
          Right f -> pure f

postFetchNewFlakeSubmitionsR :: Handler (Maybe FlakeUrl)
postFetchNewFlakeSubmitionsR = do
  Ypp { repo } <- getYesod
  requireCheckJsonBody >>= \case
    FetcherReq fetcherId Nothing fsec -> verifyFetcher fsec $ do
      $(logDebug) "Just Fetch next FlakeUrl"
      r <- popFlakeSubmition repo fetcherId
      $(logDebug) $ show $ encode r <> " :: Maybe Flake  <-> "  <> show r
      pure r
    FetcherReq fetcherId (Just fetchedFlake) fsec -> verifyFetcher fsec $ do
        $(logInfo) $ "Fetcher returned " <> show fetchedFlake
        addFetchedFlake repo fetcherId fetchedFlake

verifyFetcher :: FetcherSecret -> Handler a -> Handler a
verifyFetcher gotFsec a = do
  Ypp { repo } <- getYesod
  if gotFsec == repo.fetcherSecret
    then a
    else permissionDenied "secret mismatch"

postFindFlakesR :: Handler [ FlakeUrl ]
postFindFlakesR = do
  Ypp { repo } <- getYesod
  requireCheckJsonBody >>= go repo
  where
    go repo = findFlakes repo.repoStats repo.flakes repo.flakeIndex
