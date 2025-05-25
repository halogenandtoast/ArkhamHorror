module Arkham.Event.Events.LogicalReasoning (logicalReasoning) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Trait

newtype LogicalReasoning = LogicalReasoning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning :: EventCard LogicalReasoning
logicalReasoning = event LogicalReasoning Cards.logicalReasoning

instance RunMessage LogicalReasoning where
  runMessage msg e@(LogicalReasoning attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select =<< guardAffectsColocated iid
      chooseOrRunOneM iid do
        for_ iids \iid' -> do
          canHealHorror <- canHaveHorrorHealed attrs iid'
          terrors <- select $ TreacheryWithTrait Terror <> treacheryInThreatAreaOf iid'
          when (canHealHorror || notNull terrors) do
            targeting iid' do
              chooseOneM iid' do
                when canHealHorror $ labeled "Heal 2 Horror" $ healHorror iid' attrs 2
                unless (null terrors) $ labeled "Discard a Terror" do
                  chooseTargetM iid' terrors (toDiscardBy iid attrs)
      pure e
    _ -> LogicalReasoning <$> liftRunMessage msg attrs
