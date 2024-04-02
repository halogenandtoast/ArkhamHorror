module Arkham.Asset.Cards.TwentyFiveAutomatic2 (twentyFiveAutomatic2, TwentyFiveAutomatic2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyEvaded)
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype TwentyFiveAutomatic2 = TwentyFiveAutomatic2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twentyFiveAutomatic2 :: AssetCard TwentyFiveAutomatic2
twentyFiveAutomatic2 = asset TwentyFiveAutomatic2 Cards.twentyFiveAutomatic2

instance HasAbilities TwentyFiveAutomatic2 where
  getAbilities (TwentyFiveAutomatic2 attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1
    , controlledAbility attrs 2 (exists $ CanFightEnemy $ attrs.ability 1)
        $ freeReaction
        $ EnemyEvaded #after You
        $ EnemyAt YourLocation
    ]

instance RunMessage TwentyFiveAutomatic2 where
  runMessage msg a@(TwentyFiveAutomatic2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    ChoseEnemy iid (isAbilitySource attrs 1 -> True) eid -> do
      exhausted <- eid <=~> ExhaustedEnemy
      when exhausted do
        push $ skillTestModifiers (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      pure a
    _ -> TwentyFiveAutomatic2 <$> runMessage msg attrs
