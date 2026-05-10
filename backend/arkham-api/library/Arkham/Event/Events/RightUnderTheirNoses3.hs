module Arkham.Event.Events.RightUnderTheirNoses3 (rightUnderTheirNoses3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getsSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype RightUnderTheirNoses3 = RightUnderTheirNoses3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightUnderTheirNoses3 :: EventCard RightUnderTheirNoses3
rightUnderTheirNoses3 = event RightUnderTheirNoses3 Cards.rightUnderTheirNoses3

instance RunMessage RightUnderTheirNoses3 where
  runMessage msg e@(RightUnderTheirNoses3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      getsSkillTest skillTestResult >>= \case
        Just (SucceededBy _ n) | n >= 2 -> do
          hasConnected <-
            select
              $ connectedFrom (locationWithInvestigator iid)
              <> locationWithDiscoverableCluesBy iid
          unless (null hasConnected) do
            chooseOneM iid $ cardI18n $ scope "rightUnderTheirNoses" do
              labeled' "skip" nothing
              targets hasConnected $ discoverAt NotInvestigate iid attrs 1
        _ -> pure ()
      pure e
    _ -> RightUnderTheirNoses3 <$> liftRunMessage msg attrs
