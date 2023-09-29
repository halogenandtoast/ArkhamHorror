module Arkham.Event.Cards.WingingIt (
  wingingIt,
  WingingIt (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
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
      let modifiers = [skillTestModifier attrs iid (DiscoveredClues 1) | zone == Zone.FromDiscard]
      investigation <- mkInvestigate iid attrs
      pushAll
        $ skillTestModifier attrs lid (ShroudModifier (-1))
        : modifiers
          <> [toMessage investigation]
          <> [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs) | zone == Zone.FromDiscard]
      pure e
    _ -> WingingIt <$> runMessage msg attrs
