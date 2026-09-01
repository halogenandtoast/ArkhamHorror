module Arkham.Homebrew.CircusExMortis.Locations.PerformerTrailers (performerTrailers) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Performer))

newtype PerformerTrailers = PerformerTrailers LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

performerTrailers :: LocationCard PerformerTrailers
performerTrailers = location PerformerTrailers Cards.performerTrailers 3 (PerPlayer 2)

instance HasModifiersFor PerformerTrailers where
  getModifiersFor (PerformerTrailers a) = do
    whenAny (EnemyWithTrait Performer <> ReadyEnemy <> enemyAt (toId a)) do
      modifySelf a [ShroudModifier 1]
    whenAny (investigatorAt (toId a)) do
      modifySelect a (EnemyWithTrait Performer) [ForcePrey (Prey $ investigatorAt (toId a))]
