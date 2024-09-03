module Arkham.Asset.Cards.BrokenDiademCrownOfDyingLight5 (
  brokenDiademCrownOfDyingLight5,
  BrokenDiademCrownOfDyingLight5 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (SkillTestEnded)
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Matcher
import Arkham.Token

newtype BrokenDiademCrownOfDyingLight5 = BrokenDiademCrownOfDyingLight5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenDiademCrownOfDyingLight5 :: AssetCard BrokenDiademCrownOfDyingLight5
brokenDiademCrownOfDyingLight5 = asset BrokenDiademCrownOfDyingLight5 Cards.brokenDiademCrownOfDyingLight5

instance HasAbilities BrokenDiademCrownOfDyingLight5 where
  getAbilities (BrokenDiademCrownOfDyingLight5 x) =
    [ restrictedAbility x 1 ControlsThis
        $ freeReaction
        $ SkillTestEnded #after Anyone
        $ SkillTestAtYourLocation
        <> SkillTestWithRevealedChaosToken (IncludeTokenPool #bless)
        <> SkillTestWithRevealedChaosToken (IncludeTokenPool #curse)
    ]

instance RunMessage BrokenDiademCrownOfDyingLight5 where
  runMessage msg a@(BrokenDiademCrownOfDyingLight5 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Offering 1
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      when (attrs.use Offering >= 3) do
        twilightDiadem <- searchBondedJust iid Cards.twilightDiademCrownOfDyingLight
        push $ ReplaceInvestigatorAsset iid attrs.id twilightDiadem
        placeInBonded iid attrs
      pure a
    _ -> BrokenDiademCrownOfDyingLight5 <$> liftRunMessage msg attrs
