module Arkham.Event.Events.LogicalReasoning4 (logicalReasoning4) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype LogicalReasoning4 = LogicalReasoning4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning4 :: EventCard LogicalReasoning4
logicalReasoning4 = event LogicalReasoning4 Cards.logicalReasoning4

instance RunMessage LogicalReasoning4 where
  runMessage msg e@(LogicalReasoning4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      n <- fieldMap InvestigatorClues (min 3) iid
      repeated n $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      canHealHorror <- filterM (canHaveHorrorHealed attrs) iids
      terrors <- select $ TreacheryWithTrait Terror <> at_ (locationWithInvestigator iid)

      chooseOrRunOneM iid do
        unless (null canHealHorror) do
          labeled "Heal 2 Horror"
            $ chooseOrRunOneM iid
            $ targets canHealHorror \iid' -> healHorror iid' attrs 2
        unless (null terrors) do
          labeled "Discard a Terror" $ chooseTargetM iid terrors (toDiscardBy iid attrs)
      pure e
    _ -> LogicalReasoning4 <$> liftRunMessage msg attrs
