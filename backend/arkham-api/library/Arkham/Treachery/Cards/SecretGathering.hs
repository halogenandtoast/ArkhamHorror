module Arkham.Treachery.Cards.SecretGathering (secretGathering, SecretGathering (..)) where

import Arkham.Helpers.ChaosBag
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretGathering = SecretGathering TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretGathering :: TreacheryCard SecretGathering
secretGathering = treachery SecretGathering Cards.secretGathering

instance RunMessage SecretGathering where
  runMessage msg t@(SecretGathering attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getRemainingCurseTokens
      when (n > 0) $ addCurseTokens (Just iid) 1
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectEach (EnemyWithTrait Cultist) \enemy -> do
        placeDoom attrs enemy 1

      tokens <- count ((== #curse) . (.face)) <$> getSkillTestRevealedChaosTokens
      assignHorror iid attrs $ if tokens > 0 then 2 else 1
      pure t
    _ -> SecretGathering <$> liftRunMessage msg attrs
