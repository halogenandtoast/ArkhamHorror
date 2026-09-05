{- | Putting your own cards into someone else's deck.

An overlay is a small edit applied to a decklist before it loads: swap the
investigator for one you built (bringing their signatures with them), swap a
card for another, add cards, take cards out. It is a transformation of the
decklist itself, so everything downstream -- deck loading, investigator
creation, card generation -- carries on unchanged, resolving custom codes
through "Arkham.Card.CustomCard"'s registry the way it resolves any other.
-}
module Arkham.Custom.Overlay (
  DeckOverlay (..),
  applyOverlay,
  overlaySignatures,
  decklistCustomCards,
) where

import Arkham.Card
import Arkham.Decklist.Type
import Arkham.Id
import Arkham.Prelude
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map

data DeckOverlay = DeckOverlay
  { overlayInvestigator :: Maybe InvestigatorId
  -- ^ Replaces the deck's investigator, and their signatures with its own.
  , overlaySwaps :: Map CardCode CardCode
  -- ^ Replace one card with another, keeping the count.
  , overlayAdd :: Map CardCode Int
  , overlayRemove :: Map CardCode Int
  }
  -- 'Ord' and 'Data' because 'Message' carries one and derives both.
  deriving stock (Show, Eq, Ord, Data)

instance FromJSON DeckOverlay where
  parseJSON = withObject "DeckOverlay" \o ->
    DeckOverlay
      <$> o
      .:? "investigator"
      <*> o
      .:? "swaps"
      .!= mempty
      <*> o
      .:? "add"
      .!= mempty
      <*> o
      .:? "remove"
      .!= mempty

instance ToJSON DeckOverlay where
  toJSON o =
    object
      [ "investigator" .= overlayInvestigator o
      , "swaps" .= overlaySwaps o
      , "add" .= overlayAdd o
      , "remove" .= overlayRemove o
      ]

-- | The cards a custom investigator brings with them, from its @_signatures@ meta.
overlaySignatures :: CardDef -> [CardCode]
overlaySignatures def =
  map sanitizeCustomCardCode
    $ fromMaybe []
    $ parseMaybe parseJSON
    =<< Map.lookup "_signatures" (cdMeta def)

{- | The signature cards of whoever the deck belonged to.

Swapping the investigator has to take their signatures out, or the deck keeps
cards that were only ever theirs. A signature is a card restricted to that
investigator, which is what the deck restriction records.
-}
signaturesOf :: InvestigatorId -> Map CardCode Int -> [CardCode]
signaturesOf iid = filter restrictedToThem . Map.keys
 where
  restrictedToThem cardCode = case lookupCardDef cardCode of
    Nothing -> False
    Just def -> Signature iid `elem` cdDeckRestrictions def

applyOverlay :: DeckOverlay -> ArkhamDBDecklist -> ArkhamDBDecklist
applyOverlay overlay decklist = decklist {slots = slots', investigator_code = investigator'}
 where
  investigator' = fromMaybe (investigator_code decklist) (overlayInvestigator overlay)

  withoutOldSignatures = case overlayInvestigator overlay of
    Nothing -> slots decklist
    Just _ ->
      foldl' (flip Map.delete) (slots decklist)
        $ signaturesOf (investigator_code decklist) (slots decklist)

  withNewSignatures = case overlayInvestigator overlay of
    Nothing -> withoutOldSignatures
    Just iid ->
      foldl' (\m cardCode -> Map.insertWith (+) cardCode 1 m) withoutOldSignatures
        $ maybe [] overlaySignatures (lookupCustomCardDef (toCardCode iid))

  swapped =
    Map.foldrWithKey
      ( \from replacement m -> maybe m (\n -> Map.insertWith (+) replacement n (Map.delete from m)) (Map.lookup from m)
      )
      withNewSignatures
      (overlaySwaps overlay)

  added = Map.foldrWithKey (Map.insertWith (+)) swapped (overlayAdd overlay)

  -- Removing more than the deck holds simply takes them all out.
  slots' =
    Map.filter (> 0)
      $ Map.foldrWithKey (\cardCode n m -> Map.adjust (subtract n) cardCode m) added (overlayRemove overlay)

{- | The custom cards a decklist names, so they can be recorded on the game.

The registry a decklist resolves against is process-global and rebuilt from the
game, so a card that arrives only through an overlay would be unknown to every
other client (and to this one after a restart). Registering them onto the game
puts their defs and art where everyone reads them from.
-}
decklistCustomCards :: ArkhamDBDecklist -> [CustomCard]
decklistCustomCards decklist = mapMaybe lookupCustomCard $ toList codes
 where
  codes = go (setFromList direct) direct
  direct =
    filter isCustomCardCode
      $ toCardCode (investigator_code decklist)
      : toCardCode decklist.investigator
      : Map.keys (slots decklist)
        <> Map.keys (sideSlots decklist)

  -- A custom investigator brings signatures the decklist itself never names.
  go :: Set CardCode -> [CardCode] -> Set CardCode
  go seen [] = seen
  go seen (cardCode : rest) =
    let new = filter (`notMember` seen) $ maybe [] overlaySignatures (lookupCustomCardDef cardCode)
     in go (seen <> setFromList new) (new <> rest)
