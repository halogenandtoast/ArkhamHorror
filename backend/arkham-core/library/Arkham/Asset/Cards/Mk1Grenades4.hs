module Arkham.Asset.Cards.Mk1Grenades4 (
  mk1Grenades4,
  Mk1Grenades4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher

newtype Mk1Grenades4 = Mk1Grenades4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mk1Grenades4 :: AssetCard Mk1Grenades4
mk1Grenades4 = assetWith Mk1Grenades4 Cards.mk1Grenades4 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Mk1Grenades4 where
  getAbilities (Mk1Grenades4 a) = [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Supply 1)]

instance RunMessage Mk1Grenades4 where
  runMessage msg a@(Mk1Grenades4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifier source iid (SkillModifier #combat 2)
        , chooseFightEnemyWithTarget iid source attrs #combat
        ]
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      let
        source = toAbilitySource attrs 1
        toMsg eid' =
          targetLabel eid'
            $ if eid == eid'
              then [EnemyDamage eid' $ delayDamage $ attack source 2]
              else [EnemyDamage eid' $ delayDamage $ isDirect $ attack source 2]
      iids <- selectList $ colocatedWith iid <> NotInvestigator (InvestigatorWithId iid)
      eids <- selectList $ enemyAtLocationWith iid
      player <- getPlayer iid
      pushAll
        $ [ chooseOneAtATime player
              $ map toMsg eids
              <> [targetLabel iid' [assignDamage iid' source 2] | iid' <- iids]
          ]
        <> map (checkDefeated source) eids
      pure a
    _ -> Mk1Grenades4 <$> runMessage msg attrs
