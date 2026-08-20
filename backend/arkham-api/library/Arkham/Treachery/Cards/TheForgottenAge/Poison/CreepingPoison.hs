module Arkham.Treachery.Cards.TheForgottenAge.Poison.CreepingPoison (creepingPoison) where

import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheForgottenAge.Poison qualified as Cards
import Arkham.Treachery.CardDefs.TheForgottenAge.Poison qualified as Treacheries
import Arkham.Treachery.Import.Lifted

newtype CreepingPoison = CreepingPoison TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingPoison :: TreacheryCard CreepingPoison
creepingPoison = treachery CreepingPoison Cards.creepingPoison

instance RunMessage CreepingPoison where
  runMessage msg t@(CreepingPoison attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (HasMatchingTreachery $ treacheryIs Treacheries.poisoned) (assignDamageTo attrs 1)
      pure t
    _ -> CreepingPoison <$> liftRunMessage msg attrs
