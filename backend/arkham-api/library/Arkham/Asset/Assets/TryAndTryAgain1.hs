module Arkham.Asset.Assets.TryAndTryAgain1 (tryAndTryAgain1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (SkillCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Skill.Types (Field (..))

newtype TryAndTryAgain1 = TryAndTryAgain1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tryAndTryAgain1 :: AssetCard TryAndTryAgain1
tryAndTryAgain1 = assetWith TryAndTryAgain1 Cards.tryAndTryAgain1 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities TryAndTryAgain1 where
  getAbilities (TryAndTryAgain1 a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult #after Anyone (SkillTestWithSkill YourSkill) #failure)
          (exhaust a <> assetUseCost a Try 1)
    ]

instance RunMessage TryAndTryAgain1 where
  runMessage msg a@(TryAndTryAgain1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      committedSkillCards <- selectMapM (field SkillCard) $ skillControlledBy iid
      focusCards committedSkillCards $ chooseTargetM iid committedSkillCards (returnToHand iid)
      pure a
    _ -> TryAndTryAgain1 <$> liftRunMessage msg attrs
