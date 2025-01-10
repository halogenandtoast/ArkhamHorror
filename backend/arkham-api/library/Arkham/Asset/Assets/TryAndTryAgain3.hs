module Arkham.Asset.Assets.TryAndTryAgain3 (tryAndTryAgain3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (SkillCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Skill.Types (Field (..))

newtype TryAndTryAgain3 = TryAndTryAgain3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tryAndTryAgain3 :: AssetCard TryAndTryAgain3
tryAndTryAgain3 = asset TryAndTryAgain3 Cards.tryAndTryAgain3

instance HasAbilities TryAndTryAgain3 where
  getAbilities (TryAndTryAgain3 x) =
    [ restricted x 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult #after Anyone (SkillTestWithSkill YourSkill) #failure)
          (exhaust x)
    ]

instance RunMessage TryAndTryAgain3 where
  runMessage msg a@(TryAndTryAgain3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      committedSkillCards <- selectMapM (field SkillCard) $ skillControlledBy iid
      focusCards committedSkillCards $ chooseTargetM iid committedSkillCards (returnToHand iid)
      pure a
    _ -> TryAndTryAgain3 <$> liftRunMessage msg attrs
