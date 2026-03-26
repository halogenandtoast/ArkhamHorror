module Arkham.Event.Events.RightUnderTheirNoses (rightUnderTheirNoses) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getsSkillTest)
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype RightUnderTheirNoses = RightUnderTheirNoses EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightUnderTheirNoses :: EventCard RightUnderTheirNoses
rightUnderTheirNoses = event RightUnderTheirNoses Cards.rightUnderTheirNoses

instance RunMessage RightUnderTheirNoses where
  runMessage msg e@(RightUnderTheirNoses attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      getsSkillTest skillTestResult >>= \case
        Just (SucceededBy _ n) | n >= 2 -> do
          hasConnected <-
            select
              $ connectedFrom (locationWithInvestigator iid)
              <> locationWithDiscoverableCluesBy iid
          if null hasConnected
            then discoverAtYourLocation NotInvestigate iid attrs 1
            else chooseOneM iid do
              labeled "Discover at connected location" do
                chooseTargetM iid hasConnected $ discoverAt NotInvestigate iid attrs 1
              labeled "Do not discover at connected location" do
                discoverAtYourLocation NotInvestigate iid attrs 1
        _ -> discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    _ -> RightUnderTheirNoses <$> liftRunMessage msg attrs
