module Arkham.Enemy.Cards.MiGoDestroyer (miGoDestroyer) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey (ScenarioLogKey (TheMiGoResearchWasStopped))

newtype MiGoDestroyer = MiGoDestroyer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoDestroyer :: EnemyCard MiGoDestroyer
miGoDestroyer = enemy MiGoDestroyer Cards.miGoDestroyer

instance HasModifiersFor MiGoDestroyer where
  getModifiersFor (MiGoDestroyer a) =
    whenM (remembered TheMiGoResearchWasStopped) $ modifySelf a [HealthModifier (-2), EnemyEvade (-2)]

instance HasAbilities MiGoDestroyer where
  getAbilities (MiGoDestroyer a) =
    [ restricted a 1 (thisExists a ReadyEnemy <> exists (assetIs Assets.armoredCar))
        $ forced
        $ PhaseBegins #when #enemy
    ]

instance RunMessage MiGoDestroyer where
  runMessage msg e@(MiGoDestroyer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (assetIs Assets.armoredCar) >>= traverse_ (moveToward attrs . locationWithAsset)
      pure e
    _ -> MiGoDestroyer <$> liftRunMessage msg attrs
