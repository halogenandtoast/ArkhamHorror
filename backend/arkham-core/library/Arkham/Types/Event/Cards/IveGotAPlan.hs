module Arkham.Types.Event.Cards.IveGotAPlan
  ( iveGotAPlan
  , IveGotAPlan(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Action
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers

newtype IveGotAPlan = IveGotAPlan EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan :: InvestigatorId -> EventId -> IveGotAPlan
iveGotAPlan iid uuid = IveGotAPlan $ baseAttrs iid uuid "02107"

instance HasActions env IveGotAPlan where
  getActions iid window (IveGotAPlan attrs) = getActions iid window attrs

instance (HasCount ClueCount env InvestigatorId) => HasModifiersFor env IveGotAPlan where
  getModifiersFor (SkillTestSource iid _ _ (Just Fight)) (InvestigatorTarget _) (IveGotAPlan attrs)
    = do
      clueCount <- unClueCount <$> getCount iid
      pure $ toModifiers attrs [DamageDealt (min clueCount 3)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessage
        (ChooseFightEnemy iid (EventSource eid) SkillIntellect False)
    _ -> IveGotAPlan <$> runMessage msg attrs
