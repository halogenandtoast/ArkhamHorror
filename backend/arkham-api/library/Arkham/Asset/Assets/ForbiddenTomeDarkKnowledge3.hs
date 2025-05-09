module Arkham.Asset.Assets.ForbiddenTomeDarkKnowledge3 (forbiddenTomeDarkKnowledge3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype ForbiddenTomeDarkKnowledge3 = ForbiddenTomeDarkKnowledge3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTomeDarkKnowledge3 :: AssetCard ForbiddenTomeDarkKnowledge3
forbiddenTomeDarkKnowledge3 = asset ForbiddenTomeDarkKnowledge3 Cards.forbiddenTomeDarkKnowledge3

instance HasModifiersFor ForbiddenTomeDarkKnowledge3 where
  getModifiersFor (ForbiddenTomeDarkKnowledge3 a) = for_ a.controller \iid -> do
    selectOne (AbilityIs (toSource a) 1) >>= traverse_ \ab -> do
      handCount <- getHandCount iid
      let n = handCount `div` 4
      modifiedWhen_ a (n > 0) (AbilityTarget iid ab.ref) [ActionCostModifier (-n)]

instance HasAbilities ForbiddenTomeDarkKnowledge3 where
  getAbilities (ForbiddenTomeDarkKnowledge3 a) =
    [ controlled
        a
        1
        ( exists (EnemyAt YourLocation)
            <> AnyCriterion
              [ exists $ HealableInvestigator (toSource a) DamageType $ InvestigatorAt YourLocation
              , exists $ HealableAsset (toSource a) DamageType $ AssetAt YourLocation
              ]
        )
        $ ActionAbility []
        $ ActionCost 4
        <> exhaust a
    ]

instance RunMessage ForbiddenTomeDarkKnowledge3 where
  runMessage msg a@(ForbiddenTomeDarkKnowledge3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigators <-
        selectTargets $ HealableInvestigator (toSource attrs) DamageType $ colocatedWith iid
      assets <-
        selectTargets $ HealableAsset (toSource attrs) DamageType $ AssetAt (locationWithInvestigator iid)
      enemies <- select $ EnemyAt $ locationWithInvestigator iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
              target
              [ HealDamage target (toSource attrs) 1
              , chooseOne
                  player
                  [ targetLabel enemy [EnemyDamage enemy $ nonAttack (Just iid) (toSource attrs) 1]
                  | enemy <- enemies
                  ]
              ]
          | target <- investigators <> assets
          ]
      pure a
    _ -> ForbiddenTomeDarkKnowledge3 <$> runMessage msg attrs
