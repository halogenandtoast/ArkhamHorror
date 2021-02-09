module Arkham.Types.Treachery.Cards.GraspingHands
  ( GraspingHands(..)
  , graspingHands
  )
where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype GraspingHands = GraspingHands TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingHands :: TreacheryId -> a -> GraspingHands
graspingHands uuid _ = GraspingHands $ baseAttrs uuid "01162"

instance HasModifiersFor env GraspingHands where
  getModifiersFor = noModifiersFor

instance HasActions env GraspingHands where
  getActions i window (GraspingHands attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env GraspingHands where
  runMessage msg t@(GraspingHands attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillAgility 3
      , Discard (TreacheryTarget treacheryId)
      ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAny
          n
          0
        )
    _ -> GraspingHands <$> runMessage msg attrs
