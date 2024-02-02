module Arkham.Treachery.Cards.EphemeralExhibits (
  ephemeralExhibits,
  EphemeralExhibits (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ephemeralExhibits :: TreacheryCard EphemeralExhibits
ephemeralExhibits = treachery EphemeralExhibits Cards.ephemeralExhibits

instance RunMessage EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ beginSkillTest iid (toSource attrs) (InvestigatorTarget iid) SkillIntellect 3
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ LoseActions iid (toSource attrs) n
      pure t
    _ -> EphemeralExhibits <$> runMessage msg attrs
