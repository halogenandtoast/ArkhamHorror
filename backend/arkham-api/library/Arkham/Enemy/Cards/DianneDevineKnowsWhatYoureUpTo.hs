module Arkham.Enemy.Cards.DianneDevineKnowsWhatYoureUpTo (dianneDevineKnowsWhatYoureUpTo) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype DianneDevineKnowsWhatYoureUpTo = DianneDevineKnowsWhatYoureUpTo EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevineKnowsWhatYoureUpTo :: EnemyCard DianneDevineKnowsWhatYoureUpTo
dianneDevineKnowsWhatYoureUpTo =
  enemy DianneDevineKnowsWhatYoureUpTo Cards.dianneDevineKnowsWhatYoureUpTo (4, Static 3, 2) (1, 1)
    & setPrey MostClues

instance HasModifiersFor DianneDevineKnowsWhatYoureUpTo where
  getModifiersFor (DianneDevineKnowsWhatYoureUpTo a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n]

instance HasAbilities DianneDevineKnowsWhatYoureUpTo where
  getAbilities (DianneDevineKnowsWhatYoureUpTo a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ PerformAction #after (You <> not_ (InvestigatorAt $ locationWithEnemy a)) #parley

instance RunMessage DianneDevineKnowsWhatYoureUpTo where
  runMessage msg e@(DianneDevineKnowsWhatYoureUpTo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveToward attrs $ locationWithInvestigator iid
      pure e
    _ -> DianneDevineKnowsWhatYoureUpTo <$> liftRunMessage msg attrs
