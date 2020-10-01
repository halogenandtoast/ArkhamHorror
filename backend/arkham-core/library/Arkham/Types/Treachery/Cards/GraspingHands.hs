{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.GraspingHands where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype GraspingHands = GraspingHands Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graspingHands :: TreacheryId -> a -> GraspingHands
graspingHands uuid _ = GraspingHands $ baseAttrs uuid "01162"

instance HasActions env investigator GraspingHands where
  getActions i window (GraspingHands attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env GraspingHands where
  runMessage msg t@(GraspingHands attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ RevelationSkillTest
          iid
          (TreacherySource treacheryId)
          SkillAgility
          3
          []
          []
        , Discard (TreacheryTarget tid)
        ]
      GraspingHands <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) n 0)
    _ -> GraspingHands <$> runMessage msg attrs
