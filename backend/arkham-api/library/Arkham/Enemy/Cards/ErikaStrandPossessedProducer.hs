module Arkham.Enemy.Cards.ErikaStrandPossessedProducer (erikaStrandPossessedProducer) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.ChaosToken
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Possessed))

newtype ErikaStrandPossessedProducer = ErikaStrandPossessedProducer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erikaStrandPossessedProducer :: EnemyCard ErikaStrandPossessedProducer
erikaStrandPossessedProducer =
  enemy ErikaStrandPossessedProducer Cards.erikaStrandPossessedProducer (4, Static 5, 2) (1, 2)

instance HasAbilities ErikaStrandPossessedProducer where
  getAbilities (ErikaStrandPossessedProducer a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage ErikaStrandPossessedProducer where
  runMessage msg e@(ErikaStrandPossessedProducer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid attrs 1
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      continue_ iid
      when (any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) faces) do
        selectEach (EnemyWithTrait Possessed <> not_ (be attrs)) \possessed -> do
          readyThis possessed
          roundModifiers (attrs.ability 1) possessed [AddKeyword Hunter, RemoveKeyword Aloof]
          enemyCheckEngagement possessed
      pure e
    _ -> ErikaStrandPossessedProducer <$> liftRunMessage msg attrs
