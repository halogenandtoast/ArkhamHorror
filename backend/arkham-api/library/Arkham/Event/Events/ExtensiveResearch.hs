module Arkham.Event.Events.ExtensiveResearch (extensiveResearch) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ExtensiveResearch = ExtensiveResearch EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extensiveResearch :: EventCard ExtensiveResearch
extensiveResearch = event ExtensiveResearch Cards.extensiveResearch

instance HasModifiersFor ExtensiveResearch where
  getModifiersFor (ExtensiveResearch a) = do
    n <- fieldMap InvestigatorHand length a.owner
    modified_ a (CardIdTarget $ toCardId a) [ReduceCostOf (CardWithId $ toCardId a) n]

instance RunMessage ExtensiveResearch where
  runMessage msg e@(ExtensiveResearch attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 2
      pure e
    _ -> ExtensiveResearch <$> liftRunMessage msg attrs
