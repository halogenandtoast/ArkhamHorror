module Arkham.Event.Events.WillOfTheCosmos (willOfTheCosmos) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype WillOfTheCosmos = WillOfTheCosmos EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willOfTheCosmos :: EventCard WillOfTheCosmos
willOfTheCosmos = event WillOfTheCosmos Cards.willOfTheCosmos

instance RunMessage WillOfTheCosmos where
  runMessage msg e@(WillOfTheCosmos attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ TargetControlledBy (InvestigatorWithId iid)
        <> NotTarget ScenarioCardTarget
      pure e
    HandleTargetChoice iid (isSource attrs -> True) t -> do
      batched \_ -> do
        placeDoom attrs t 1
        discoverAtYourLocation NotInvestigate iid attrs 1
        discoverAtMatchingLocation
          NotInvestigate
          iid
          attrs
          (#revealed <> not_ (locationWithInvestigator iid))
          1
      pure e
    _ -> WillOfTheCosmos <$> liftRunMessage msg attrs
