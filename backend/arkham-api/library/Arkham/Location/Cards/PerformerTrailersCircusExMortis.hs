module Arkham.Location.Cards.PerformerTrailersCircusExMortis (performerTrailersCircusExMortis) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Performer))

newtype PerformerTrailersCircusExMortis = PerformerTrailersCircusExMortis LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

performerTrailersCircusExMortis :: LocationCard PerformerTrailersCircusExMortis
performerTrailersCircusExMortis =
  location PerformerTrailersCircusExMortis Cards.performerTrailersCircusExMortis 3 (PerPlayer 2)

instance HasModifiersFor PerformerTrailersCircusExMortis where
  getModifiersFor (PerformerTrailersCircusExMortis a) = do
    -- +1 shroud while there are 1+ ready Performer enemies here
    readyPerformerHere <- selectAny $ EnemyWithTrait Performer <> ReadyEnemy <> enemyAt (toId a)
    when readyPerformerHere $ modifySelf a [ShroudModifier 1]
    -- while 1+ investigators here, each Performer enemy preys on investigators here
    investigatorsHere <- selectAny $ investigatorAt (toId a)
    when investigatorsHere
      $ modifySelect a (EnemyWithTrait Performer) [ForcePrey (Prey $ investigatorAt (toId a))]

instance RunMessage PerformerTrailersCircusExMortis where
  runMessage msg (PerformerTrailersCircusExMortis attrs) = runQueueT $ case msg of
    _ -> PerformerTrailersCircusExMortis <$> liftRunMessage msg attrs
