module Arkham.Asset.Assets.PoliceDog (policeDog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestInvestigator, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype PoliceDog = PoliceDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeDog :: AssetCard PoliceDog
policeDog = ally PoliceDog Cards.policeDog (1, 1)

instance HasAbilities PoliceDog where
  getAbilities (PoliceDog a) =
    [ controlled
        a
        1
        (DuringSkillTest $ SkillTestAtYourLocation <> oneOf [WhileAttacking, WhileInvestigating Anywhere])
        $ FastAbility (exhaust a)
    ]

instance RunMessage PoliceDog where
  runMessage msg a@(PoliceDog attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        withSkillTestInvestigator \iid' ->
          skillTestModifier sid (attrs.ability 1) iid' (AnySkillValue 1)
      pure a
    _ -> PoliceDog <$> liftRunMessage msg attrs
