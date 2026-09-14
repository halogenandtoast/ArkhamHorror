{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Deck (
  module Entity.Arkham.Deck,
) where

import Arkham.Custom.Overlay (DeckOverlay, applyOverlay)
import Arkham.Decklist
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Time.Clock (UTCTime)
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity.Arkham.ArkhamDBDecklist ()
import Entity.Arkham.DeckOverlay ()
import Entity.User
import Json
import Orphans ()
import Relude

{- | @overlay@ holds the custom cards laid over the deck. It sits beside the
list rather than being folded into it, so the original deck stays intact and
the overlay can be lifted again.

@lastUsedAt@ is when the deck was last taken into a game -- see
'Entity.Answer.touchDeck'. Null for a deck that has never been played.
-}
share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamDeck sql=arkham_decks
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  url Text Maybe
  name Text
  investigatorName Text
  list ArkhamDBDecklist
  overlay DeckOverlay Maybe
  lastUsedAt UTCTime Maybe
  deriving Generic Show
|]

{- | @playList@ rides along so the client shows the deck as it will be played
without having to reimplement 'applyOverlay'.
-}
instance ToJSON ArkhamDeck where
  toJSON deck = case genericToJSON (aesonOptions $ Just "arkhamDeck") deck of
    Object o -> Object $ KeyMap.insert "playList" (toJSON $ arkhamDeckPlayList deck) o
    v -> v

-- | The deck as it should be played: the stored list with its overlay applied.
arkhamDeckPlayList :: ArkhamDeck -> ArkhamDBDecklist
arkhamDeckPlayList deck =
  maybe id applyOverlay (arkhamDeckOverlay deck) (arkhamDeckList deck)
