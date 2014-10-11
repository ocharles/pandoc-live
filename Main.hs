{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Enumerator.List (generateM)

import qualified Blaze.ByteString.Builder as ByteBulider
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Text.Pandoc as Pandoc
import qualified System.Linux.Inotify as INotify
import qualified ServerSentEvent as SSE
import qualified Snap.Core as Snap
import qualified Snap.Iteratee as Snap
import qualified Snap.Http.Server as Snap
import qualified Control.Concurrent.STM as STM
import qualified Snap.Util.FileServe as Snap

import Paths_pandoc_live

main :: IO ()
main = do
  events <- STM.newBroadcastTChanIO
  [ f ] <- getArgs

  inotify <- INotify.init
  watch <- INotify.addWatch inotify f (INotify.in_MODIFY)
  watcher <- Async.async $ forever $ do
    e <- INotify.getEvent inotify
    when (INotify.wd e == watch) $ do
      doc <- Pandoc.readMarkdown Pandoc.def <$> readFile f
      STM.atomically (STM.writeTChan events doc)

  Async.link watcher

  indexFile <- getDataFileName "index.html"
  Snap.quickHttpServe $
    Snap.route [("/events", sinkEvents events),
                ("/", Snap.serveFile indexFile)]

sinkEvents :: STM.TChan Pandoc.Pandoc -> Snap.Snap ()
sinkEvents chan = do
  chan' <- liftIO (STM.atomically (STM.dupTChan chan))
  Snap.modifyResponse $
    Snap.setContentType "text/event-stream" .
    Snap.setResponseBody
      (generateM (Just . docToBuilder <$>
                  STM.atomically (STM.readTChan chan')))
  where
  docToBuilder doc =
    ByteBulider.fromByteString
      (Text.encodeUtf8 $ LText.toStrict $ Builder.toLazyText $
       SSE.eventToBuilder $
       SSE.ServerEvent
         { SSE.eventName =
             Nothing
         , SSE.eventId =
             Nothing
         , SSE.eventData =
             [Text.concatMap
                (\c ->
                   if c == '\n'
                      then "\\n"
                      else Text.singleton c) $
              Text.pack $
              Pandoc.writeHtmlString
                (Pandoc.def {Pandoc.writerHTMLMathMethod =
                                 Pandoc.MathJax mathJaxJS})
                doc]
         }) <>
    ByteBulider.flush

  mathJaxJS = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
