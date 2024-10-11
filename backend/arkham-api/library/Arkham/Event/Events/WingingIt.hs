module Arkham.Event.Events.WingingIt (wingingIt, WingingIt (..)) where

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Zone qualified as Zone

newtype WingingIt = WingingIt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingingIt :: EventCard WingingIt
wingingIt = event WingingIt Cards.wingingIt

instance RunMessage WingingIt where
  runMessage msg e@(WingingIt attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      sid <- getRandom
      modifiers <-
        if zone == Zone.FromDiscard
          then (: []) <$> skillTestModifier sid attrs iid (DiscoveredClues 1)
          else pure []
      investigation <- mkInvestigate sid iid attrs
      enabled <- skillTestModifier sid attrs lid (ShroudModifier (-1))
      pushAll
        $ enabled
        : modifiers
          <> [toMessage investigation]
          <> [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs) | zone == Zone.FromDiscard]
      pure e
    _ -> WingingIt <$> runMessage msg attrs
