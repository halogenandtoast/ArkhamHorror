module Arkham.Location.Cards.WitchesCircle (
  witchesCircle,
  WitchesCircle (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Witch))

newtype WitchesCircle = WitchesCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

witchesCircle :: LocationCard WitchesCircle
witchesCircle = location WitchesCircle Cards.witchesCircle 3 (PerPlayer 3)

enemyMatcher :: LocationId -> EnemyMatcher
enemyMatcher lid = UnengagedEnemy <> ReadyEnemy <> EnemyWithTrait Witch <> NotEnemy (enemyAt lid)

instance HasAbilities WitchesCircle where
  getAbilities (WitchesCircle a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (enemyExists $ enemyMatcher $ toId a)
            $ ForcedAbility
            $ PhaseBegins #after #enemy
        ]

instance RunMessage WitchesCircle where
  runMessage msg l@(WitchesCircle attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      anetteMason <- getSetAsideCard Enemies.anetteMason
      pushM $ createEnemyAt_ anetteMason (toId attrs) Nothing
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLeadPlayer
      enemiesToMove <- selectList $ enemyMatcher (toId attrs)

      pushWhen (notNull enemiesToMove)
        $ chooseOneAtATime lead
        $ targetLabels enemiesToMove
        $ \enemy -> only $ MoveToward (toTarget enemy) (LocationWithId $ toId attrs)
      pure l
    _ -> WitchesCircle <$> runMessage msg attrs
