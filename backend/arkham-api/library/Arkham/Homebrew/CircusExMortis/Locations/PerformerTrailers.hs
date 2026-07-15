module Arkham.Homebrew.CircusExMortis.Locations.PerformerTrailers (performerTrailers) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Performer))

newtype PerformerTrailers = PerformerTrailers LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

performerTrailers :: LocationCard PerformerTrailers
performerTrailers =
  location PerformerTrailers Cards.performerTrailers 3 (PerPlayer 2)

instance HasModifiersFor PerformerTrailers where
  getModifiersFor (PerformerTrailers a) = do
    -- +1 shroud while there are 1+ ready Performer enemies here
    readyPerformerHere <- selectAny $ EnemyWithTrait Performer <> ReadyEnemy <> enemyAt (toId a)
    when readyPerformerHere $ modifySelf a [ShroudModifier 1]
    -- while 1+ investigators here, each Performer enemy preys on investigators here
    investigatorsHere <- selectAny $ investigatorAt (toId a)
    when investigatorsHere
      $ modifySelect a (EnemyWithTrait Performer) [ForcePrey (Prey $ investigatorAt (toId a))]

instance RunMessage PerformerTrailers where
  runMessage msg (PerformerTrailers attrs) = runQueueT $ case msg of
    _ -> PerformerTrailers <$> liftRunMessage msg attrs
