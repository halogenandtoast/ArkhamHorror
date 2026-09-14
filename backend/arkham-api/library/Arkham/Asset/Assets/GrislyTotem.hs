module Arkham.Asset.Assets.GrislyTotem (grislyTotem) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getCommittedCard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Queue (QueueT)
import Arkham.SkillType

newtype GrislyTotem = GrislyTotem AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotem :: AssetCard GrislyTotem
grislyTotem = asset GrislyTotem Cards.grislyTotem

instance HasAbilities GrislyTotem where
  getAbilities (GrislyTotem a) =
    [controlled_ a 1 $ triggered (CommittedCard #after You #any) (exhaust a)]

skillIconLabeled :: ReverseQueue m => SkillIcon -> QueueT Message m () -> ChooseT m ()
skillIconLabeled WildMinusIcon body = withI18n $ labeled "chooseMinusWild" body
skillIconLabeled icon body = withI18n $ skillIconVar icon $ labeled "chooseSkillIcon" body

instance RunMessage GrislyTotem where
  runMessage msg a@(GrislyTotem attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
      withSkillTest \sid -> do
        chooseOrRunOneM iid do
          for_ (setToList icons) \icon -> do
            skillIconLabeled icon do
              skillTestModifier sid attrs card (AddSkillIcons [icon])
      pure a
    _ -> GrislyTotem <$> liftRunMessage msg attrs
