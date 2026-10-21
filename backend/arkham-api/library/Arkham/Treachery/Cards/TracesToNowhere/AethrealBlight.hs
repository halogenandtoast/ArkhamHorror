module Arkham.Treachery.Cards.TracesToNowhere.AethrealBlight (aethrealBlight) where

import Arkham.Card
import Arkham.Helpers (Deck (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AethrealBlight = AethrealBlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aethrealBlight :: TreacheryCard AethrealBlight
aethrealBlight = treachery AethrealBlight Cards.aethrealBlight

instance RunMessage AethrealBlight where
  runMessage msg t@(AethrealBlight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOneM iid $ scenarioI18n do
        labeledValidate (notNull assets) "aethrealBlight.discardAsset" do
          chooseTargetM iid assets $ toDiscardBy iid attrs
        labeled "aethrealBlight.removeFromGame" $ doStep 1 msg
      pure t
    DoStep 1 (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      cards <- fieldMap InvestigatorDeck (take 2 . map toCard . unDeck) iid
      for_ cards \card ->
        if cardIsWeakness card
          then shuffleCardsIntoDeck iid [card]
          else removeCardFromGame card
      pure t
    _ -> AethrealBlight <$> liftRunMessage msg attrs
