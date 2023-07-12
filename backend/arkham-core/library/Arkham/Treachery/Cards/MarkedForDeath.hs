module Arkham.Treachery.Cards.MarkedForDeath
  ( markedForDeath
  , MarkedForDeath(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkedForDeath = MarkedForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForDeath :: TreacheryCard MarkedForDeath
markedForDeath = treachery MarkedForDeath Cards.markedForDeath

instance RunMessage MarkedForDeath where
  runMessage msg t@(MarkedForDeath attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MarkedForDeath <$> runMessage msg attrs
