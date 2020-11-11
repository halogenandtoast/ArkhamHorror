{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.GraspingHands where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype GraspingHands = GraspingHands Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graspingHands :: TreacheryId -> a -> GraspingHands
graspingHands uuid _ = GraspingHands $ baseAttrs uuid "01162"

instance HasModifiersFor env GraspingHands where
  getModifiersFor = noModifiersFor

instance HasActions env GraspingHands where
  getActions i window (GraspingHands attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env GraspingHands where
  runMessage msg t@(GraspingHands attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessages
        [ RevelationSkillTest iid source SkillAgility 3 [] [] []
        , Discard (TreacheryTarget treacheryId)
        ]
      GraspingHands <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) n 0)
    _ -> GraspingHands <$> runMessage msg attrs
