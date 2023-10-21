module Arkham.Treachery.Cards.DrivenToMadness
  ( drivenToMadness
  , DrivenToMadness(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DrivenToMadness = DrivenToMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drivenToMadness :: TreacheryCard DrivenToMadness
drivenToMadness = treachery DrivenToMadness Cards.drivenToMadness

instance RunMessage DrivenToMadness where
  runMessage msg t@(DrivenToMadness attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DrivenToMadness <$> runMessage msg attrs
