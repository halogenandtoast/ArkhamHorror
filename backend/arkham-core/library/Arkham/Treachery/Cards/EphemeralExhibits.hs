module Arkham.Treachery.Cards.EphemeralExhibits (ephemeralExhibits, EphemeralExhibits (..)) where

import Arkham.Classes
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ephemeralExhibits :: TreacheryCard EphemeralExhibits
ephemeralExhibits = treachery EphemeralExhibits Cards.ephemeralExhibits

instance RunMessage EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #intellect 3
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ LoseActions iid (toSource attrs) n
      pure t
    _ -> EphemeralExhibits <$> runMessage msg attrs
