module Arkham.Asset.Cards.ForbiddenTomeDarkKnowledge3
  ( forbiddenTomeDarkKnowledge3
  , ForbiddenTomeDarkKnowledge3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Target

newtype ForbiddenTomeDarkKnowledge3 = ForbiddenTomeDarkKnowledge3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTomeDarkKnowledge3 :: AssetCard ForbiddenTomeDarkKnowledge3
forbiddenTomeDarkKnowledge3 =
  asset ForbiddenTomeDarkKnowledge3 Cards.forbiddenTomeDarkKnowledge3

instance HasModifiersFor ForbiddenTomeDarkKnowledge3 where
  getModifiersFor (AbilityTarget iid ab) (ForbiddenTomeDarkKnowledge3 a)
    | isSource a (abilitySource ab) && abilityIndex ab == 1 = do
      handCount <- getHandCount iid
      let n = handCount `div` 4
      pure $ toModifiers a [ ActionCostModifier (-n) | n > 0 ]
  getModifiersFor _ _ = pure []

instance HasAbilities ForbiddenTomeDarkKnowledge3 where
  getAbilities (ForbiddenTomeDarkKnowledge3 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis
          <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation)
          <> AnyCriterion
               [ InvestigatorExists
                 (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
               , AssetExists (AssetAt YourLocation <> AssetWithDamage)
               ]
          )
        $ ActionAbility Nothing
        $ ActionCost 4
        <> ExhaustCost (toTarget a)
    ]

instance RunMessage ForbiddenTomeDarkKnowledge3 where
  runMessage msg a@(ForbiddenTomeDarkKnowledge3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigators <-
        selectListMap InvestigatorTarget
        $ colocatedWith iid
        <> InvestigatorWithAnyDamage
      assets <-
        selectListMap AssetTarget
        $ AssetAt (locationWithInvestigator iid)
        <> AssetWithDamage
      enemies <- selectListMap EnemyTarget $ EnemyAt $ locationWithInvestigator
        iid
      push $ chooseOne
        iid
        [ TargetLabel
            target
            [ HealDamage target (toSource attrs) 1
            , chooseOne
              iid
              [ TargetLabel enemy [Damage enemy (toSource attrs) 1]
              | enemy <- enemies
              ]
            ]
        | target <- investigators <> assets
        ]
      pure a
    _ -> ForbiddenTomeDarkKnowledge3 <$> runMessage msg attrs
