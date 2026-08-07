module Arkham.Location.Cards.FrontPorchEntryway (frontPorchEntryway, FrontPorchEntryway (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Scenarios.AThousandShapesOfHorror.Helpers

newtype FrontPorchEntryway = FrontPorchEntryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frontPorchEntryway :: LocationCard FrontPorchEntryway
frontPorchEntryway = location FrontPorchEntryway Cards.frontPorchEntryway 2 (PerPlayer 1)

instance HasAbilities FrontPorchEntryway where
  getAbilities (FrontPorchEntryway attrs) =
    extendRevealed
      attrs
      [ scenarioI18n
          ( withI18nTooltip "frontPorchEntryway.revealUpstairs"
              $ restrictedAbility attrs 1 (Here <> exists (UnrevealedLocation <> "Upstairs Hallway"))
              $ FastAbility
              $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
          )
      , scenarioI18n
          ( withI18nTooltip "frontPorchEntryway.placeUnmarkedTomb"
              $ onlyOnce
              $ restrictedAbility
                attrs
                2
                ( Here
                    <> exists (enemyIs Enemies.theUnnamable <> EnemyWithDamage (AtLeast $ PerPlayer 1))
                    -- The Library's ability places the same one Unmarked Tomb, so
                    -- normally whichever fires first hides the other. With
                    -- achievements on, both stay offerable (once each) so "Déjà Vu"
                    -- can tick both boxes; the second one resolves as a no-op.
                    <> oneOf [AchievementsEnabled, notExists (LocationWithTitle "Unmarked Tomb")]
                )
              $ FastAbility Free
          )
      ]

instance RunMessage FrontPorchEntryway where
  runMessage msg l@(FrontPorchEntryway attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      upstairsHallway <- selectJust $ LocationWithTitle "Upstairs Hallway"
      push $ Msg.RevealLocation (Just iid) upstairsHallway
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      alreadyPlaced <- selectAny $ LocationWithTitle "Unmarked Tomb"
      unless alreadyPlaced $ push $ PlaceLocationMatching "Unmarked Tomb"
      pure l
    _ -> FrontPorchEntryway <$> runMessage msg attrs
