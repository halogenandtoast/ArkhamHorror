module Arkham.Treachery.Cards.NoTurningBack
  ( noTurningBack
  , NoTurningBack(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

instance RunMessage NoTurningBack where
  runMessage msg t@(NoTurningBack attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> NoTurningBack <$> runMessage msg attrs
