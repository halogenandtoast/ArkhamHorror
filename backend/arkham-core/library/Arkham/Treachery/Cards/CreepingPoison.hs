module Arkham.Treachery.Cards.CreepingPoison (
  creepingPoison,
  CreepingPoison (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Runner

newtype CreepingPoison = CreepingPoison TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

creepingPoison :: TreacheryCard CreepingPoison
creepingPoison = treachery CreepingPoison Cards.creepingPoison

instance RunMessage CreepingPoison where
  runMessage msg t@(CreepingPoison attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      iids <-
        selectList
          $ HasMatchingTreachery
          $ treacheryIs
            Treacheries.poisoned
      pushAll
        [InvestigatorAssignDamage iid source DamageAny 1 0 | iid <- iids]
      pure t
    _ -> CreepingPoison <$> runMessage msg attrs
