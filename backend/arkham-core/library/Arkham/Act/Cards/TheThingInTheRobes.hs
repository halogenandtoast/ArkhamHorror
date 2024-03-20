module Arkham.Act.Cards.TheThingInTheRobes (TheThingInTheRobes (..), theThingInTheRobes) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype TheThingInTheRobes = TheThingInTheRobes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingInTheRobes :: ActCard TheThingInTheRobes
theThingInTheRobes = act (2, A) TheThingInTheRobes Cards.theThingInTheRobes Nothing

instance HasAbilities TheThingInTheRobes where
  getAbilities (TheThingInTheRobes attrs) =
    [ restrictedAbility
        attrs
        1
        ( Remembered ManeuveredThePriestCloser
            <> Remembered StunnedThePriest
            <> exists (enemyIs Enemies.highPriestNotToBeDescribed)
        )
        actionAbility
    , mkAbility attrs 2
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Enemies.highPriestNotToBeDescribed
    ]

instance RunMessage TheThingInTheRobes where
  runMessage msg a@(TheThingInTheRobes attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- glad we don't care about anything here
      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      priest <- selectJust $ enemyIs Enemies.highPriestNotToBeDescribed
      defeatEnemy priest iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      advanceVia #other attrs iid
      pure a
    _ -> TheThingInTheRobes <$> lift (runMessage msg attrs)
