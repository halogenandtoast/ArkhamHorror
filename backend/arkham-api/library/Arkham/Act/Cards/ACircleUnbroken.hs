module Arkham.Act.Cards.ACircleUnbroken (aCircleUnbroken) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype ACircleUnbroken = ACircleUnbroken ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCircleUnbroken :: ActCard ACircleUnbroken
aCircleUnbroken = act (4, A) ACircleUnbroken Cards.aCircleUnbroken Nothing

instance HasAbilities ACircleUnbroken where
  getAbilities (ACircleUnbroken x) =
    [ mkAbility x 1 $ Objective $ forced $ ifEnemyDefeated Enemies.anetteMason
    , restricted x 2 (exists $ locationIs Locations.witchesCircle <> LocationWithoutClues)
        $ Objective (forced AnyWindow)
    ]

instance RunMessage ACircleUnbroken where
  runMessage msg a@(ACircleUnbroken attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      defeatedAnette <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.anetteMason
      push $ if defeatedAnette then R1 else R2
      pure a
    _ -> ACircleUnbroken <$> liftRunMessage msg attrs
