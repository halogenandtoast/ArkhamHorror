module Arkham.Event.Cards.ExtensiveResearch (
  extensiveResearch,
  ExtensiveResearch (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ExtensiveResearch = ExtensiveResearch EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

extensiveResearch :: EventCard ExtensiveResearch
extensiveResearch = event ExtensiveResearch Cards.extensiveResearch

instance HasModifiersFor ExtensiveResearch where
  getModifiersFor (CardIdTarget cid) (ExtensiveResearch a) | toCardId a == cid =
    do
      n <- fieldMap InvestigatorHand length (eventOwner a)
      pure $ toModifiers a [ReduceCostOf (CardWithId cid) n]
  getModifiersFor _ _ = pure []

instance RunMessage ExtensiveResearch where
  runMessage msg e@(ExtensiveResearch attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 2 Nothing
      pure e
    _ -> ExtensiveResearch <$> runMessage msg attrs
