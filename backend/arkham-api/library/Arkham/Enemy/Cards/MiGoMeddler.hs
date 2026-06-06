module Arkham.Enemy.Cards.MiGoMeddler (miGoMeddler) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype MiGoMeddler = MiGoMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoMeddler :: EnemyCard MiGoMeddler
miGoMeddler = enemy MiGoMeddler Cards.miGoMeddler (3, PerPlayer 2, 4) (1, 0)

instance HasAbilities MiGoMeddler where
  getAbilities (MiGoMeddler a) = [restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy]

instance RunMessage MiGoMeddler where
  runMessage msg e@(MiGoMeddler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      atExplosives <- selectAny $ locationWithEnemy attrs <> LocationWithAnyHorror
      if atExplosives
        then selectEach (assetIs Assets.theMilitarysPlan) \aid -> removeTokens (attrs.ability 1) aid #damage 1
        else push $ MoveToward (toTarget attrs) LocationWithAnyHorror
      pure e
    _ -> MiGoMeddler <$> liftRunMessage msg attrs
