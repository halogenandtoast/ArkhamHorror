module Arkham.Asset.Assets.OccultLexicon where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCanShuffleDeck, searchBonded)
import Arkham.Matcher

newtype OccultLexicon = OccultLexicon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon :: AssetCard OccultLexicon
occultLexicon = asset OccultLexicon Cards.occultLexicon

instance HasAbilities OccultLexicon where
  getAbilities (OccultLexicon a) =
    [restricted a 1 ControlsThis $ forced $ AssetEntersPlay #after (be a)]

instance RunMessage OccultLexicon where
  runMessage msg (OccultLexicon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      bonded <- take 3 <$> searchBonded iid Events.bloodRite
      for_ (nonEmpty bonded) \(handBloodRite :| deckBloodRites) -> do
        addToHand iid (only handBloodRite)
        whenM (getCanShuffleDeck iid) $ shuffleCardsIntoDeck iid deckBloodRites
      OccultLexicon <$> liftRunMessage msg attrs
    RemovedFromPlay (isSource attrs -> True) -> do
      for_ attrs.owner \iid -> push (RemoveAllCopiesOfCardFromGame iid "05317")
      OccultLexicon <$> liftRunMessage msg attrs
    _ -> OccultLexicon <$> liftRunMessage msg attrs
