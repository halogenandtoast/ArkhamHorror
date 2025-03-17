module Arkham.Event.Events.ExtensiveResearch1 (extensiveResearch1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ExtensiveResearch1 = ExtensiveResearch1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extensiveResearch1 :: EventCard ExtensiveResearch1
extensiveResearch1 = event ExtensiveResearch1 Cards.extensiveResearch1

instance HasModifiersFor ExtensiveResearch1 where
  getModifiersFor (ExtensiveResearch1 a) = do
    n <- fieldMap InvestigatorHand length a.owner
    modified_ a a.cardId [ReduceCostOf (CardWithId a.cardId) n]

instance RunMessage ExtensiveResearch1 where
  runMessage msg e@(ExtensiveResearch1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid (toSource attrs) 2
      pure e
    _ -> ExtensiveResearch1 <$> liftRunMessage msg attrs
