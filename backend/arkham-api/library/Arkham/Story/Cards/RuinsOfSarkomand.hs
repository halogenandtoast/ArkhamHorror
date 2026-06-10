module Arkham.Story.Cards.RuinsOfSarkomand (ruinsOfSarkomand) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RuinsOfSarkomand = RuinsOfSarkomand StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfSarkomand :: StoryCard RuinsOfSarkomand
ruinsOfSarkomand = story RuinsOfSarkomand Cards.ruinsOfSarkomand

instance RunMessage RuinsOfSarkomand where
  runMessage msg s@(RuinsOfSarkomand attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sid <- getRandom
      campaignI18n $ chooseOneM iid do
        labeled' "ruinsOfSarkomand.speakToTheDenizens"
          $ beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
        labeled' "ruinsOfSarkomand.blockTheTrapdoor"
          $ beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember WarnedTheDenizensOfSarkomand
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember CutOffAllEscape
      pure s
    _ -> RuinsOfSarkomand <$> liftRunMessage msg attrs
