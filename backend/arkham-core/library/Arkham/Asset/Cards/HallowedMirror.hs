module Arkham.Asset.Cards.HallowedMirror where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck
import Arkham.Event.Cards qualified as Events

newtype HallowedMirror = HallowedMirror AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallowedMirror :: AssetCard HallowedMirror
hallowedMirror = asset HallowedMirror Cards.hallowedMirror

instance RunMessage HallowedMirror where
  runMessage msg (HallowedMirror attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      handSoothingMelody <- PlayerCard <$> genPlayerCard Events.soothingMelody
      deckSoothingMelodies <- replicateM 2 (genCard Events.soothingMelody)
      canShuffleDeck <- getCanShuffleDeck iid
      pushAll $
        addToHand iid handSoothingMelody
          : [ShuffleCardsIntoDeck (InvestigatorDeck iid) deckSoothingMelodies | canShuffleDeck]
      HallowedMirror <$> runMessage msg attrs
    RemovedFromPlay source | isSource attrs source -> do
      let iid = getOwner attrs
      push (RemoveAllCopiesOfCardFromGame iid "05314")
      HallowedMirror <$> runMessage msg attrs
    _ -> HallowedMirror <$> runMessage msg attrs
