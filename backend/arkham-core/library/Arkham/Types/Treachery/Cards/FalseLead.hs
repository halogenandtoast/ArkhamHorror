module Arkham.Types.Treachery.Cards.FalseLead where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

instance TreacheryRunner env => RunMessage env FalseLead where
  runMessage msg t@(FalseLead attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- unClueCount <$> getCount iid
      if playerClueCount == 0
        then
          t
            <$ pushAll
                 [ chooseOne iid [Surge iid (toSource attrs)]
                 , Discard $ toTarget attrs
                 ]
        else t <$ pushAll
          [ RevelationSkillTest iid source SkillIntellect 4
          , Discard $ toTarget attrs
          ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t <$ push (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
