{-# LANGUAGE OverloadedStrings #-}
module ServerSentEvent (ServerEvent(..), eventToBuilder) where

import Data.Foldable
import Data.Monoid
import Data.Text
import Data.Text.Lazy.Builder

data ServerEvent
  = ServerEvent {eventName :: Maybe Text
                ,eventId :: Maybe Text
                ,eventData :: [Text]}
  | CommentEvent {eventComment :: Text}
  | RetryEvent {eventRetry :: Int}

nl :: Builder
nl = "\n"

field :: Builder -> Builder -> Builder
field l b = l <> b <> nl

eventToBuilder :: ServerEvent -> Builder
eventToBuilder (CommentEvent txt) =
  field ":" (fromText txt)
eventToBuilder (RetryEvent n) =
  field "retry:" (fromText . pack . show $ n)
eventToBuilder (ServerEvent n i d) =
  (name n
        (evid i
              (foldMap (field "data:" .
                        fromText)
                       d))) `mappend`
  nl
  where
  name Nothing = id
  name (Just n') =
    mappend (field "event:" (fromText n'))
  evid Nothing = id
  evid (Just i') =
    mappend (field "id:" (fromText i'))
