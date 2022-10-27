module Arkham.Treachery.Cards.OutOfBodyExperience
  ( outOfBodyExperience
  , OutOfBodyExperience(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype OutOfBodyExperience = OutOfBodyExperience TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfBodyExperience :: TreacheryCard OutOfBodyExperience
outOfBodyExperience = treachery OutOfBodyExperience Cards.outOfBodyExperience

instance RunMessage OutOfBodyExperience where
  runMessage msg t@(OutOfBodyExperience attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> OutOfBodyExperience <$> runMessage msg attrs
