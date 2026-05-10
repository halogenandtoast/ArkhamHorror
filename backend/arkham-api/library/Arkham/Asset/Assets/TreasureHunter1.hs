module Arkham.Asset.Assets.TreasureHunter1 (treasureHunter1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Prelude

newtype TreasureHunter1 = TreasureHunter1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treasureHunter1 :: AssetCard TreasureHunter1
treasureHunter1 = ally TreasureHunter1 Cards.treasureHunter1 (2, 2)

instance HasModifiersFor TreasureHunter1 where
  getModifiersFor (TreasureHunter1 a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities TreasureHunter1 where
  getAbilities (TreasureHunter1 x) = [restrictedAbility x 1 ControlsThis $ ForcedAbility $ PhaseEnds #when #upkeep]

instance RunMessage TreasureHunter1 where
  runMessage msg a@(TreasureHunter1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label (withI18n $ cardNameVar attrs $ ikey' "label.payResourceToName") [SpendResources iid 1]
          , Label (withI18n $ cardNameVar attrs $ ikey' "label.discardName") [toDiscardBy iid (toAbilitySource attrs 1) attrs]
          ]
      pure a
    _ -> TreasureHunter1 <$> runMessage msg attrs
