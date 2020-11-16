{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.FalseLead where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype FalseLead = FalseLead Attrs
  deriving newtype (Show, ToJSON, FromJSON)

falseLead :: TreacheryId -> a -> FalseLead
falseLead uuid _ = FalseLead $ baseAttrs uuid "01136"

instance HasModifiersFor env FalseLead where
  getModifiersFor = noModifiersFor

instance HasActions env FalseLead where
  getActions i window (FalseLead attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FalseLead where
  runMessage msg t@(FalseLead attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- asks $ unClueCount . getCount iid
      if playerClueCount == 0
        then unshiftMessage (Ask iid $ ChooseOne [Surge iid (toSource attrs)])
        else unshiftMessage (RevelationSkillTest iid source SkillIntellect 4)
      FalseLead <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} n
      | tid == treacheryId -> t
      <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
