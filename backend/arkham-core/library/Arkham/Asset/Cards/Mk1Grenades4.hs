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
import Arkham.SkillType

newtype Mk1Grenades4 = Mk1Grenades4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mk1Grenades4 :: AssetCard Mk1Grenades4
mk1Grenades4 =
  assetWith Mk1Grenades4 Cards.mk1Grenades4 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Mk1Grenades4 where
  getAbilities (Mk1Grenades4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
          <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage Mk1Grenades4 where
  runMessage msg a@(Mk1Grenades4 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillCombat 2)
              , ChooseFightEnemy
                  iid
                  source
                  (Just $ toTarget attrs)
                  SkillCombat
                  mempty
                  False
              ]
    Successful (Action.Fight, EnemyTarget eid) iid _ target _
      | isTarget attrs target -> do
          let
            toMsg eid' =
              targetLabel
                eid'
                if eid == eid'
                  then [EnemyDamage eid' $ delayDamage $ attack attrs 2]
                  else [EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 2]
          iids <-
            selectList
              $ colocatedWith iid
                <> NotInvestigator
                  (InvestigatorWithId iid)
          msgs <- selectListMap toMsg $ EnemyAt (locationWithInvestigator iid)
          pushAll
            [ chooseOneAtATime
                iid
                ( msgs
                    <> [ targetLabel
                        iid'
                        [ InvestigatorAssignDamage
                            iid'
                            (toSource attrs)
                            DamageAny
                            2
                            0
                        ]
                       | iid' <- iids
                       ]
                )
            , CheckDefeated (toSource attrs)
            ]
          pure a
    _ -> Mk1Grenades4 <$> runMessage msg attrs
