module Arkham.Treachery.Cards.ConspiracyOfDeepOnes (conspiracyOfDeepOnes, ConspiracyOfDeepOnes (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.IntoTheMaelstrom.Helpers (scenarioI18n)
import Arkham.Trait (Trait (AncientOne, Sanctum))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ConspiracyOfDeepOnes = ConspiracyOfDeepOnes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conspiracyOfDeepOnes :: TreacheryCard ConspiracyOfDeepOnes
conspiracyOfDeepOnes = treachery ConspiracyOfDeepOnes Cards.conspiracyOfDeepOnes

instance RunMessage ConspiracyOfDeepOnes where
  runMessage msg t@(ConspiracyOfDeepOnes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + n)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      nearest <-
        select
          $ NearestEnemyToFallback iid
          $ withTrait AncientOne
          <> mapOneOf enemyIs [Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom, Enemies.hydraAwakenedAndEnraged]
      chooseOneM iid $ scenarioI18n $ scope "conspiracyOfDeepOnes" do
        labeled' "placeDoomCanAdvance"
          $ placeDoomOnAgendaAndCheckAdvance 1
        when (notNull nearest) do
          labeled' "ancientOneAttacks" do
            chooseTargetM iid nearest \enemy -> initiateEnemyAttack enemy attrs iid

      pure t
    _ -> ConspiracyOfDeepOnes <$> liftRunMessage msg attrs
