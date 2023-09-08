module Arkham.Treachery.Cards.AWorldInDarkness
  ( aWorldInDarkness
  , AWorldInDarkness(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AWorldInDarkness = AWorldInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWorldInDarkness :: TreacheryCard AWorldInDarkness
aWorldInDarkness = treachery AWorldInDarkness Cards.aWorldInDarkness

instance RunMessage AWorldInDarkness where
  runMessage msg t@(AWorldInDarkness attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AWorldInDarkness <$> runMessage msg attrs
