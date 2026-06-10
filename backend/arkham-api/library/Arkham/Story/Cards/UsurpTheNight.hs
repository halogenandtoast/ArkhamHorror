module Arkham.Story.Cards.UsurpTheNight (usurpTheNight) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Message (pattern R3)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UsurpTheNight = UsurpTheNight StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

usurpTheNight :: StoryCard UsurpTheNight
usurpTheNight = story UsurpTheNight Cards.usurpTheNight

instance RunMessage UsurpTheNight where
  runMessage msg s@(UsurpTheNight attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      strength <- getStrengthOfTheAbyss
      requirementsMet <-
        andM
          [ pure $ strength >= 5
          , remembered ExecutedTheNightgaunts
          , remembered CutOffAllEscape
          , remembered AffrontedTheRulerOfThisRealm
          ]
      if requirementsMet
        then campaignI18n $ chooseOneM iid do
          labeled' "usurpTheNight.proceed" $ push R3
          labeled' "usurpTheNight.flipBack" nothing
        else do
          investigators <- allInvestigators
          for_ investigators \iid' -> do
            campaignI18n $ chooseOneM iid' do
              labeled' "usurpTheNight.acceptMercy" $ resign iid'
              labeled' "usurpTheNight.refuse" nothing
      pure s
    _ -> UsurpTheNight <$> liftRunMessage msg attrs
