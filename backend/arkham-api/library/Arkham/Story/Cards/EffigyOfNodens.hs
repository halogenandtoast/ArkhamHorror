module Arkham.Story.Cards.EffigyOfNodens (effigyOfNodens) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EffigyOfNodens = EffigyOfNodens StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

effigyOfNodens :: StoryCard EffigyOfNodens
effigyOfNodens = story EffigyOfNodens Cards.effigyOfNodens

instance RunMessage EffigyOfNodens where
  runMessage msg s@(EffigyOfNodens attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sid <- getRandom
      campaignI18n $ chooseOneM iid do
        labeled' "effigyOfNodens.entreatForAid"
          $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
        labeled' "effigyOfNodens.overthrowTheStatue"
          $ beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 2)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember PledForHelp
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember AffrontedTheRulerOfThisRealm
      pure s
    _ -> EffigyOfNodens <$> liftRunMessage msg attrs
