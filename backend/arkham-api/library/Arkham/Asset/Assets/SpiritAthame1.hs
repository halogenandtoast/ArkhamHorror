module Arkham.Asset.Assets.SpiritAthame1 (spiritAthame1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype SpiritAthame1 = SpiritAthame1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritAthame1 :: AssetCard SpiritAthame1
spiritAthame1 = asset SpiritAthame1 Cards.spiritAthame1

instance HasAbilities SpiritAthame1 where
  getAbilities (SpiritAthame1 x) =
    [ controlled x 1 (DuringSkillTest (SkillTestSourceMatches $ SourceWithTrait Spell))
        $ FastAbility (exhaust x)
    , restricted x 2 ControlsThis $ fightAction (exhaust x)
    ]

instance RunMessage SpiritAthame1 where
  runMessage msg a@(SpiritAthame1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> SpiritAthame1 <$> liftRunMessage msg attrs
