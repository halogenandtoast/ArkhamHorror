module Arkham.Asset.Cards.GarroteWire2 (garroteWire2, GarroteWire2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude

newtype GarroteWire2 = GarroteWire2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garroteWire2 :: AssetCard GarroteWire2
garroteWire2 = asset GarroteWire2 Cards.garroteWire2

instance HasAbilities GarroteWire2 where
  getAbilities (GarroteWire2 attrs) =
    [ controlledAbility
        attrs
        1
        (DuringTurn You <> exists (CanFightEnemy (toSource attrs) <> EnemyWithRemainingHealth (static 1)))
        $ FastAbility' (exhaust attrs) [#fight]
    ]

instance RunMessage GarroteWire2 where
  runMessage msg a@(GarroteWire2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFightMatch iid source (EnemyWithRemainingHealth $ static 1)
      pushAll [skillTestModifier source iid (SkillModifier #combat 2), chooseFight]
      pure a
    _ -> GarroteWire2 <$> runMessage msg attrs
