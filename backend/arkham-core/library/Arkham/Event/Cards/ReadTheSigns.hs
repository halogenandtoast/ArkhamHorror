module Arkham.Event.Cards.ReadTheSigns (
  readTheSigns,
  ReadTheSigns (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Message
import Arkham.Projection

newtype ReadTheSigns = ReadTheSigns EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readTheSigns :: EventCard ReadTheSigns
readTheSigns = event ReadTheSigns Cards.readTheSigns

instance RunMessage ReadTheSigns where
  runMessage msg e@(ReadTheSigns attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ skillTestModifiers
            attrs
            iid
            [AddSkillValue #willpower, DiscoveredClues 1, MayIgnoreLocationEffectsAndKeywords]
        , Investigate iid lid (toSource attrs) Nothing skillType False
        ]
      pure e
    _ -> ReadTheSigns <$> runMessage msg attrs
