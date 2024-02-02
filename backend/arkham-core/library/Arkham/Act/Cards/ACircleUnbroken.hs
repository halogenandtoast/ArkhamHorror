module Arkham.Act.Cards.ACircleUnbroken (ACircleUnbroken (..), aCircleUnbroken) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude

newtype ACircleUnbroken = ACircleUnbroken ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

aCircleUnbroken :: ActCard ACircleUnbroken
aCircleUnbroken = act (4, A) ACircleUnbroken Cards.aCircleUnbroken Nothing

instance HasAbilities ACircleUnbroken where
  getAbilities (ACircleUnbroken x) =
    [ mkAbility x 1
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Enemies.anetteMason
    , restrictedAbility x 2 (exists $ locationIs Locations.witchesCircle <> LocationWithoutClues)
        $ Objective (forced AnyWindow)
    ]

instance RunMessage ACircleUnbroken where
  runMessage msg a@(ACircleUnbroken attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ advanceVia #other attrs attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ advanceVia #other attrs attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      defeatedAnette <- selectAny $ VictoryDisplayCardMatch $ cardIs Enemies.anetteMason
      push $ if defeatedAnette then R1 else R2
      pure a
    _ -> ACircleUnbroken <$> runMessage msg attrs
