module Arkham.Event.Cards.IveGotAPlan
  ( iveGotAPlan
  , IveGotAPlan(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Action
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype IveGotAPlan = IveGotAPlan EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan :: EventCard IveGotAPlan
iveGotAPlan = event IveGotAPlan Cards.iveGotAPlan

instance (HasCount ClueCount env InvestigatorId) => HasModifiersFor env IveGotAPlan where
  getModifiersFor (SkillTestSource iid _ _ (Just Fight)) (InvestigatorTarget _) (IveGotAPlan attrs)
    = do
      clueCount <- unClueCount <$> getCount iid
      pure $ toModifiers attrs [DamageDealt (min clueCount 3)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e <$ push
        (ChooseFightEnemy
          iid
          (EventSource eid)
          Nothing
          SkillIntellect
          mempty
          False
        )
    _ -> IveGotAPlan <$> runMessage msg attrs
