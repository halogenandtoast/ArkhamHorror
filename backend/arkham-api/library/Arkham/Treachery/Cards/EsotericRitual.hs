module Arkham.Treachery.Cards.EsotericRitual (esotericRitual, EsotericRitual (..)) where

import Arkham.Discard
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EsotericRitual = EsotericRitual TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericRitual :: TreacheryCard EsotericRitual
esotericRitual = treachery EsotericRitual Cards.esotericRitual

instance RunMessage EsotericRitual where
  runMessage msg t@(EsotericRitual attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      tokens <- count ((== #curse) . (.face)) <$> getSkillTestRevealedChaosTokens
      hasDiscardableAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      hasDiscardableCard <- selectAny $ inHandOf iid <> basic DiscardableCard
      chooseNM iid (if tokens > 0 then 2 else 1) do
        when hasDiscardableCard
          $ labeled "Discard 2 cards from your hand"
          $ discardFromHand iid attrs DiscardChoose 2

        when hasDiscardableAssets
          $ labeled "Discard an asset you control"
          $ chooseAndDiscardAsset iid attrs
      pure t
    _ -> EsotericRitual <$> liftRunMessage msg attrs
