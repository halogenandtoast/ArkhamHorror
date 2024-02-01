module Arkham.Event.Cards.FortuitousDiscovery (
  fortuitousDiscovery,
  FortuitousDiscovery (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher

newtype FortuitousDiscovery = FortuitousDiscovery EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fortuitousDiscovery :: EventCard FortuitousDiscovery
fortuitousDiscovery =
  event FortuitousDiscovery Cards.fortuitousDiscovery

instance RunMessage FortuitousDiscovery where
  runMessage msg e@(FortuitousDiscovery attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      x <-
        selectCount
          $ InDiscardOf (InvestigatorWithId iid)
          <> BasicCardMatch (cardIs Cards.fortuitousDiscovery)
      pushAll
        [ skillTestModifiers (toSource attrs) iid [SkillModifier #intellect x, DiscoveredClues x]
        , toMessage investigation
        ]
      pure e
    _ -> FortuitousDiscovery <$> runMessage msg attrs
