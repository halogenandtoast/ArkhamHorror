module Arkham.Story.Cards.PrisonersOfConquest (prisonersOfConquest) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PrisonersOfConquest = PrisonersOfConquest StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prisonersOfConquest :: StoryCard PrisonersOfConquest
prisonersOfConquest = story PrisonersOfConquest Cards.prisonersOfConquest

instance RunMessage PrisonersOfConquest where
  runMessage msg s@(PrisonersOfConquest attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sid <- getRandom
      campaignI18n $ chooseOneM iid do
        labeled' "prisonersOfConquest.freeTheCreatures"
          $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
        labeled' "prisonersOfConquest.slayTheCreatures"
          $ beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenJustM (getSetAsideCardMaybe Assets.summonedNightgaunt) \card ->
        takeControlOfSetAsideAsset iid card
      remember FreedTheNightgaunts
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember ExecutedTheNightgaunts
      pure s
    _ -> PrisonersOfConquest <$> liftRunMessage msg attrs
