module Arkham.Event.Cards.Pilfer
  ( pilfer
  , Pilfer(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (InvestigatorLocation) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType

newtype Pilfer = Pilfer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilfer :: EventCard Pilfer
pilfer = event Pilfer Cards.pilfer

instance RunMessage Pilfer where
  runMessage msg e@(Pilfer attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      pushAll
        [ skillTestModifier attrs (toTarget iid) (DiscoveredClues 2)
        , Investigate iid lid (toSource attrs) Nothing SkillAgility False
        ]
      pure e
    _ -> Pilfer <$> runMessage msg attrs
