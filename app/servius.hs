{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Main (main) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import           CMarkGFM                           (extAutolink, extTable, extStrikethrough, optSmart, commonmarkToHtml)
import qualified Data.ByteString.Char8              as S8
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8With)
import           Data.Text.Encoding.Error           (lenientDecode)
import qualified Data.Text.Lazy                     as TL
import           Network.HTTP.Types                 (status200)
import           Network.Wai                        (Middleware, Response,
                                                     pathInfo, responseBuilder)
import           Text.Blaze.Html                    (preEscapedToHtml)
import           Text.Blaze.Html.Renderer.Utf8      (renderHtmlBuilder)
import           Text.Hamlet                        (defaultHamletSettings, shamlet)
import           Text.Hamlet.RT                     (parseHamletRT,
                                                     renderHamletRT)
import           Text.Lucius                        (luciusRT)
import           WaiAppStatic.CmdLine               (docroot, runCommandLine)

main :: IO ()
main = runCommandLine (shake . docroot)

shake :: FilePath -> Middleware
shake docroot app req respond
    | any unsafe p = app req respond
    | null p = app req respond
    | ".hamlet" `T.isSuffixOf` l = hamlet pr >>= respond
    | ".lucius" `T.isSuffixOf` l = lucius pr >>= respond
    | ".markdown" `T.isSuffixOf` l = markdown' pr >>= respond
    | ".md" `T.isSuffixOf` l = markdown' pr >>= respond
    | otherwise = app req respond
  where
    p = pathInfo req
    pr = T.intercalate "/" $ T.pack docroot : p
    l = last p

unsafe :: Text -> Bool
unsafe s
    | T.null s = False
    | T.head s == '.' = True
    | otherwise = T.any (== '/') s

readFileUtf8 :: Text -> IO String
readFileUtf8 fp = do
    bs <- S8.readFile $ T.unpack fp
    let t = decodeUtf8With lenientDecode bs
    return $ T.unpack t

hamlet :: Text -> IO Response
hamlet fp = do
    str <- readFileUtf8 fp
    hrt <- parseHamletRT defaultHamletSettings str
    html <- renderHamletRT hrt [] (error "No URLs allowed")
    return $ responseBuilder status200 [("Content-Type", "text/html; charset=utf-8")] $ renderHtmlBuilder html

lucius :: Text -> IO Response
lucius fp = do
    str <- readFileUtf8 fp
    let text = either error id $ luciusRT (TL.pack str) []
    return $ responseBuilder status200 [("Content-Type", "text/css; charset=utf-8")] $ fromLazyText text

markdown' :: Text -> IO Response
markdown' fp = do
    bs <- S8.readFile $ T.unpack fp
    let t = decodeUtf8With lenientDecode bs
        html = commonmarkToHtml
          [optSmart]
          [extStrikethrough, extTable, extAutolink]
          t
        title = T.strip $ T.dropWhile (== '#') $ T.concat $ take 1 $ dropWhile T.null $ T.lines t
    return $ responseBuilder status200 [("Content-Type", "text/html; charset=utf-8")] $ renderHtmlBuilder
      [shamlet|
          $doctype 5
          <html>
            <head>
              <meta charset=utf-8>
              <meta http-equiv="X-UA-Compatible" content="IE=edge">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>#{title}

              <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
              <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/zenburn.min.css">


            <body>
              <div .container>
                <div .row>
                  <div .col-sm-2>
                  <div .col-sm-8>
                    <article>#{preEscapedToHtml html}

              <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js">
              <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js">
              <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/rust.min.js">
              <script>hljs.initHighlightingOnLoad();
      |]
