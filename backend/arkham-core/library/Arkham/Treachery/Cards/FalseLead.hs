module Arkham.Treachery.Cards.FalseLead where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance RunMessage FalseLead where
  runMessage msg t@(FalseLead attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- field InvestigatorClues iid
      push $ if playerClueCount == 0
        then chooseOne iid [Label "Surge" [Surge iid (toSource attrs)]]
        else RevelationSkillTest iid source SkillIntellect 4
      pure t
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == toId attrs -> t <$ push (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
