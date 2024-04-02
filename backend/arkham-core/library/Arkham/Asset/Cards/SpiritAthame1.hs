module Arkham.Asset.Cards.SpiritAthame1 (spiritAthame1, SpiritAthame1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype SpiritAthame1 = SpiritAthame1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritAthame1 :: AssetCard SpiritAthame1
spiritAthame1 = asset SpiritAthame1 Cards.spiritAthame1

instance HasAbilities SpiritAthame1 where
  getAbilities (SpiritAthame1 x) =
    [ controlledAbility x 1 (DuringSkillTest (SkillTestSourceMatches $ SourceWithTrait Spell))
        $ FastAbility (exhaust x)
    , restrictedAbility x 2 ControlsThis $ fightAction (exhaust x)
    ]

instance RunMessage SpiritAthame1 where
  runMessage msg a@(SpiritAthame1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifier source iid (SkillModifier #combat 2)
        , chooseFight
        ]
      pure a
    _ -> SpiritAthame1 <$> runMessage msg attrs
