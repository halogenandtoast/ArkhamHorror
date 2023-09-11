module Arkham.Treachery.Cards.RottingRemains where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RottingRemains = RottingRemains TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rottingRemains :: TreacheryCard RottingRemains
rottingRemains = treachery RottingRemains Cards.rottingRemains

instance RunMessage RottingRemains where
  runMessage msg t@(RottingRemains attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower 3
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ assignHorror iid attrs n
      pure t
    _ -> RottingRemains <$> runMessage msg attrs
