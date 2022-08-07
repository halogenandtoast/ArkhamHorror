module Arkham.Treachery.Cards.FalseLead where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance RunMessage FalseLead where
  runMessage msg t@(FalseLead attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- field InvestigatorClues iid
      if playerClueCount == 0
        then t <$ push (chooseOne iid [Surge iid (toSource attrs)])
        else t <$ push (RevelationSkillTest iid source SkillIntellect 4)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == toId attrs -> t <$ push (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
