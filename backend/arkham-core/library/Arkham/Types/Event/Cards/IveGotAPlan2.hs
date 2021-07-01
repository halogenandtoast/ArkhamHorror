module Arkham.Types.Event.Cards.IveGotAPlan2
  ( iveGotAPlan2
  , IveGotAPlan2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype IveGotAPlan2 = IveGotAPlan2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan2 :: EventCard IveGotAPlan2
iveGotAPlan2 = event IveGotAPlan2 Cards.iveGotAPlan2

instance HasActions env IveGotAPlan2 where
  getActions iid window (IveGotAPlan2 attrs) = getActions iid window attrs

instance (HasCount ClueCount env InvestigatorId) => HasModifiersFor env IveGotAPlan2 where
  getModifiersFor (SkillTestSource iid _ _ _ (Just Fight)) (InvestigatorTarget _) (IveGotAPlan2 attrs)
    = do
      clueCount <- unClueCount <$> getCount iid
      pure $ toModifiers
        attrs
        [DamageDealt (min clueCount 3), SkillModifier SkillIntellect 2]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env IveGotAPlan2 where
  runMessage msg e@(IveGotAPlan2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessage
        (ChooseFightEnemy iid (EventSource eid) SkillIntellect mempty False)
    _ -> IveGotAPlan2 <$> runMessage msg attrs
