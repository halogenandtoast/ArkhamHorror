module Arkham.Event.Cards.ReadTheSigns (
  readTheSigns,
  ReadTheSigns (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate

newtype ReadTheSigns = ReadTheSigns EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readTheSigns :: EventCard ReadTheSigns
readTheSigns = event ReadTheSigns Cards.readTheSigns

instance RunMessage ReadTheSigns where
  runMessage msg e@(ReadTheSigns attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid attrs
      pushAll
        [ skillTestModifiers
            sid
            attrs
            iid
            [AddSkillValue #willpower, DiscoveredClues 1, MayIgnoreLocationEffectsAndKeywords]
        , toMessage investigation
        ]
      pure e
    _ -> ReadTheSigns <$> runMessage msg attrs
