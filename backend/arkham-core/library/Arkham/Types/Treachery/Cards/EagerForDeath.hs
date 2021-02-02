module Arkham.Types.Treachery.Cards.EagerForDeath
  ( EagerForDeath(..)
  , eagerForDeath
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype EagerForDeath = EagerForDeath Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

eagerForDeath :: TreacheryId -> a -> EagerForDeath
eagerForDeath uuid _ = EagerForDeath $ baseAttrs uuid "02091"

instance HasModifiersFor env EagerForDeath where
  getModifiersFor = noModifiersFor

instance HasActions env EagerForDeath where
  getActions i window (EagerForDeath attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env EagerForDeath where
  runMessage msg t@(EagerForDeath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      difficulty <- (+ 2) . unDamageCount <$> getCount iid
      t <$ unshiftMessages
        [ RevelationSkillTest iid source SkillWillpower difficulty
        , Discard (toTarget attrs)
        ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t
      <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> EagerForDeath <$> runMessage msg attrs
