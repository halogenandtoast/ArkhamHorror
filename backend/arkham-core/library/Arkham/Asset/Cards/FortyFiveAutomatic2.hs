module Arkham.Asset.Cards.FortyFiveAutomatic2 (
  fortyFiveAutomatic2,
  FortyFiveAutomatic2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Keyword (Keyword (Retaliate))
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype FortyFiveAutomatic2 = FortyFiveAutomatic2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fortyFiveAutomatic2 :: AssetCard FortyFiveAutomatic2
fortyFiveAutomatic2 = asset FortyFiveAutomatic2 Cards.fortyFiveAutomatic2

instance HasAbilities FortyFiveAutomatic2 where
  getAbilities (FortyFiveAutomatic2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          ([Action.Fight])
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage FortyFiveAutomatic2 where
  runMessage msg a@(FortyFiveAutomatic2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [DamageDealt 1, SkillModifier SkillCombat 2, IgnoreRetaliate]
        , ChooseFightEnemy iid source (Just $ toTarget attrs) SkillCombat mempty False
        ]
      pure a
    Successful (Action.Fight, EnemyTarget eid) _ _ (isTarget attrs -> True) _ -> do
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      ignored <- member eid <$> select (EnemyWithKeyword Retaliate)
      pushAll $ EnemyDamage eid (attack attrs 1) : [ignoreWindow | ignored]
      pure a
    _ -> FortyFiveAutomatic2 <$> runMessage msg attrs
