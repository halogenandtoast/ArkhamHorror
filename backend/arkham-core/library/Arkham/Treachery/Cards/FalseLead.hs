module Arkham.Treachery.Cards.FalseLead where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance RunMessage FalseLead where
  runMessage msg t@(FalseLead attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      playerClueCount <- field InvestigatorClues iid
      player <- getPlayer iid
      push
        $ if playerClueCount == 0
          then chooseOne player [Label "Surge" [gainSurge attrs]]
          else revelationSkillTest iid attrs #intellect 4
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) n
      pure t
    _ -> FalseLead <$> runMessage msg attrs
