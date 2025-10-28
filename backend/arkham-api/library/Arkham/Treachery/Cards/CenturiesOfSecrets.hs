module Arkham.Treachery.Cards.CenturiesOfSecrets (centuriesOfSecrets) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Trait (Trait (Curse))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CenturiesOfSecrets = CenturiesOfSecrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centuriesOfSecrets :: TreacheryCard CenturiesOfSecrets
centuriesOfSecrets = treachery CenturiesOfSecrets Cards.centuriesOfSecrets

instance RunMessage CenturiesOfSecrets where
  runMessage msg t@(CenturiesOfSecrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeckAndHandle iid attrs n attrs
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      when (any (`cardMatch` CardWithTrait Curse) cards) $ do
        directDamage iid attrs 1
        assets <- select $ assetControlledBy iid <> AllyAsset
        for_ assets \asset -> dealAssetDirectDamage asset attrs 1
      pure t
    _ -> CenturiesOfSecrets <$> liftRunMessage msg attrs
