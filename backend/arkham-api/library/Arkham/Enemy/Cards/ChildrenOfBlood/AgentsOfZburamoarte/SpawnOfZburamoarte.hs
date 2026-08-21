module Arkham.Enemy.Cards.ChildrenOfBlood.AgentsOfZburamoarte.SpawnOfZburamoarte (spawnOfZburamoarte) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SpawnOfZburamoarte = SpawnOfZburamoarte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spawnOfZburamoarte :: EnemyCard SpawnOfZburamoarte
spawnOfZburamoarte =
  enemy SpawnOfZburamoarte Cards.spawnOfZburamoarte
    & setPrey (InvestigatorWithMostSealedChaosToken #blood)

instance HasAbilities SpawnOfZburamoarte where
  getAbilities (SpawnOfZburamoarte a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealsDamage #after (be a)

instance RunMessage SpawnOfZburamoarte where
  runMessage msg e@(SpawnOfZburamoarte attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> SpawnOfZburamoarte <$> liftRunMessage msg attrs
