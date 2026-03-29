module Arkham.Asset.Assets.PoliceDog1 (policeDog1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype PoliceDog1 = PoliceDog1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeDog1 :: AssetCard PoliceDog1
policeDog1 = ally PoliceDog1 Cards.policeDog1 (1, 1)

instance HasAbilities PoliceDog1 where
  getAbilities (PoliceDog1 a) =
    [ controlled
        a
        1
        ( DuringSkillTest
            $ SkillTestAtYourLocation
            <> oneOf [WhileAttacking, WhileInvestigating Anywhere]
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage PoliceDog1 where
  runMessage msg a@(PoliceDog1 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        getSkillTestInvestigator >>= traverse_ \iid' ->
          skillTestModifier sid (attrs.ability 1) iid' (AnySkillValue 2)
      pure a
    _ -> PoliceDog1 <$> liftRunMessage msg attrs
