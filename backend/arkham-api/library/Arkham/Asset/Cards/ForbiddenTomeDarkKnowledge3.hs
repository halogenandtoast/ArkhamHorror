module Arkham.Asset.Cards.ForbiddenTomeDarkKnowledge3 (
  forbiddenTomeDarkKnowledge3,
  ForbiddenTomeDarkKnowledge3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype ForbiddenTomeDarkKnowledge3 = ForbiddenTomeDarkKnowledge3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTomeDarkKnowledge3 :: AssetCard ForbiddenTomeDarkKnowledge3
forbiddenTomeDarkKnowledge3 =
  asset ForbiddenTomeDarkKnowledge3 Cards.forbiddenTomeDarkKnowledge3

instance HasModifiersFor ForbiddenTomeDarkKnowledge3 where
  getModifiersFor (AbilityTarget iid ab) (ForbiddenTomeDarkKnowledge3 a)
    | isSource a (abilitySource ab) && abilityIndex ab == 1 = do
        handCount <- getHandCount iid
        let n = handCount `div` 4
        pure $ toModifiers a [ActionCostModifier (-n) | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities ForbiddenTomeDarkKnowledge3 where
  getAbilities (ForbiddenTomeDarkKnowledge3 a) =
    [ controlledAbility
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
                [ targetLabel enemy [EnemyDamage enemy $ nonAttack (toSource attrs) 1]
                | enemy <- enemies
                ]
            ]
          | target <- investigators <> assets
          ]
      pure a
    _ -> ForbiddenTomeDarkKnowledge3 <$> runMessage msg attrs
