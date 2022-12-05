module Arkham.Treachery.Cards.CreepingDarkness
  ( creepingDarkness
  , CreepingDarkness(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CreepingDarkness = CreepingDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingDarkness :: TreacheryCard CreepingDarkness
creepingDarkness = treachery CreepingDarkness Cards.creepingDarkness

instance RunMessage CreepingDarkness where
  runMessage msg t@(CreepingDarkness attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> CreepingDarkness <$> runMessage msg attrs
