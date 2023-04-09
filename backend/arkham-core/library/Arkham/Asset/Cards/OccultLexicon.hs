module Arkham.Asset.Cards.OccultLexicon where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Event.Cards qualified as Events
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck

newtype OccultLexicon = OccultLexicon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon :: AssetCard OccultLexicon
occultLexicon = asset OccultLexicon Cards.occultLexicon

instance RunMessage OccultLexicon where
  runMessage msg (OccultLexicon attrs) = case msg of
    InvestigatorPlayAsset iid aid | aid == assetId attrs -> do
      handBloodRite <- PlayerCard <$> genPlayerCard Events.bloodRite
      deckBloodRites <- replicateM 2 (genCard Events.bloodRite)
      canShuffleDeck <- getCanShuffleDeck iid
      pushAll
        $ addToHand iid handBloodRite
        : [ ShuffleCardsIntoDeck (InvestigatorDeck iid) deckBloodRites | canShuffleDeck ]
      OccultLexicon <$> runMessage msg attrs
    RemovedFromPlay source | isSource attrs source -> do
      let iid = getOwner attrs
      push (RemoveAllCopiesOfCardFromGame iid "05317")
      OccultLexicon <$> runMessage msg attrs
    _ -> OccultLexicon <$> runMessage msg attrs
