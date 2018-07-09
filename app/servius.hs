{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Main (main) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import           CMarkGFM                           (extAutolink, extTable, extStrikethrough, optSmart, commonmarkToHtml)
import qualified Data.ByteString.Char8              as S8
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8Builder, decodeUtf8With)
import           Data.Text.Encoding.Error           (lenientDecode)
import qualified Data.Text.Lazy                     as TL
import           Network.HTTP.Types                 (status200)
import           Network.Wai                        (Middleware, Response,
                                                     pathInfo, responseBuilder)
import           Text.Blaze.Html.Renderer.Utf8      (renderHtmlBuilder)
import           Text.Hamlet                        (defaultHamletSettings)
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
    return $ responseBuilder status200 [("Content-Type", "text/html; charset=utf-8")] $ encodeUtf8Builder html
