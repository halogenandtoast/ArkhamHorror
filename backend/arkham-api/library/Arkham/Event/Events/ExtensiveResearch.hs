module Arkham.Event.Events.ExtensiveResearch (
  extensiveResearch,
  ExtensiveResearch (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype ExtensiveResearch = ExtensiveResearch EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extensiveResearch :: EventCard ExtensiveResearch
extensiveResearch = event ExtensiveResearch Cards.extensiveResearch

instance HasModifiersFor ExtensiveResearch where
  getModifiersFor (ExtensiveResearch a) = do
    n <- fieldMap InvestigatorHand length (eventOwner a)
    modified_ a (CardIdTarget $ toCardId a) [ReduceCostOf (CardWithId $ toCardId a) n]

instance RunMessage ExtensiveResearch where
  runMessage msg e@(ExtensiveResearch attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toSource attrs) 2
      pure e
    _ -> ExtensiveResearch <$> runMessage msg attrs
