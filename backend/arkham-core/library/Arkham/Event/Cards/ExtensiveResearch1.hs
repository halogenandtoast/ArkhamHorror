module Arkham.Event.Cards.ExtensiveResearch1 (
  extensiveResearch1,
  ExtensiveResearch1 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ExtensiveResearch1 = ExtensiveResearch1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

extensiveResearch1 :: EventCard ExtensiveResearch1
extensiveResearch1 = event ExtensiveResearch1 Cards.extensiveResearch1

instance HasModifiersFor ExtensiveResearch1 where
  getModifiersFor (CardIdTarget cid) (ExtensiveResearch1 a) | toCardId a == cid =
    do
      n <- fieldMap InvestigatorHand length (eventOwner a)
      pure $ toModifiers a [ReduceCostOf (CardWithId cid) n]
  getModifiersFor _ _ = pure []

instance RunMessage ExtensiveResearch1 where
  runMessage msg e@(ExtensiveResearch1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 2 Nothing
      pure e
    _ -> ExtensiveResearch1 <$> runMessage msg attrs
