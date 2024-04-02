module Arkham.Asset.Cards.FortyFiveAutomatic2 (fortyFiveAutomatic2, FortyFiveAutomatic2 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Fight
import Arkham.Keyword (Keyword (Retaliate))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

newtype FortyFiveAutomatic2 = FortyFiveAutomatic2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic2 :: AssetCard FortyFiveAutomatic2
fortyFiveAutomatic2 = asset FortyFiveAutomatic2 Cards.fortyFiveAutomatic2

instance HasAbilities FortyFiveAutomatic2 where
  getAbilities (FortyFiveAutomatic2 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveAutomatic2 where
  runMessage msg a@(FortyFiveAutomatic2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseFight <- setTarget attrs <$> mkChooseFight iid attrs
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 1, SkillModifier #combat 2, IgnoreRetaliate]
        , toMessage chooseFight
        ]
      pure a
    Successful (Action.Fight, EnemyTarget eid) _ _ (isTarget attrs -> True) _ -> do
      ignoreWindow <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      ignored <- eid <=~> EnemyWithKeyword Retaliate
      pushAll $ EnemyDamage eid (attack attrs 1) : [ignoreWindow | ignored]
      pure a
    _ -> FortyFiveAutomatic2 <$> runMessage msg attrs
