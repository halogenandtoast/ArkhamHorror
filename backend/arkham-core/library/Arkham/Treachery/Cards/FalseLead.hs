module Arkham.Treachery.Cards.FalseLead where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance TreacheryRunner env => RunMessage env FalseLead where
  runMessage msg t@(FalseLead attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- unClueCount <$> getCount iid
      if playerClueCount == 0
        then t <$ push (chooseOne iid [Surge iid (toSource attrs)])
        else t <$ push (RevelationSkillTest iid source SkillIntellect 4)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == toId attrs -> t <$ push (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
