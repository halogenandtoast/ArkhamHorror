module Arkham.Asset.Cards.FirstAid3 (
  firstAid3,
  FirstAid3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype FirstAid3 = FirstAid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid3 :: AssetCard FirstAid3
firstAid3 = assetWith FirstAid3 Cards.firstAid3 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities FirstAid3 where
  getAbilities (FirstAid3 x) =
    [ controlledAbility
        x
        1
        ( AnyCriterion
            [ exists
                $ oneOf
                  [ HealableInvestigator (toSource x) HorrorType $ InvestigatorAt YourLocation
                  , HealableInvestigator (toSource x) DamageType $ InvestigatorAt YourLocation
                  ]
            , exists
                $ oneOf
                  [ HealableAsset (toSource x) HorrorType $ AssetAt YourLocation
                  , HealableAsset (toSource x) DamageType $ AssetAt YourLocation
                  ]
            ]
        )
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage FirstAid3 where
  runMessage msg a@(FirstAid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let
        componentLabel component target = case target of
          InvestigatorTarget iid' ->
            ComponentLabel (InvestigatorComponent iid' component)
          AssetTarget aid -> ComponentLabel (AssetComponent aid component)
          _ -> error "unhandled target"

      player <- getPlayer iid

      assetChoices <- do
        horrorAssets <-
          select
            $ HealableAsset (toSource attrs) HorrorType
            $ AssetAt
            $ locationWithInvestigator iid
        damageAssets <-
          select
            $ HealableAsset (toSource attrs) DamageType
            $ AssetAt
            $ locationWithInvestigator iid
        let allAssets = setToList $ horrorAssets <> damageAssets
        pure $ flip map allAssets $ \asset' ->
          let target = AssetTarget asset'
           in targetLabel
                asset'
                [ chooseOneAtATime player
                    $ [ componentLabel
                        DamageToken
                        target
                        [HealDamage target (toSource attrs) 1]
                      | asset' `member` damageAssets
                      ]
                    <> [ componentLabel
                        HorrorToken
                        target
                        [HealHorror target (toSource attrs) 1]
                       | asset' `member` horrorAssets
                       ]
                ]

      investigatorChoices <- do
        horrorInvestigators <-
          select
            $ HealableInvestigator (toSource attrs) HorrorType
            $ colocatedWith iid
        damageInvestigators <-
          select
            $ HealableInvestigator (toSource attrs) DamageType
            $ colocatedWith iid
        let
          allInvestigators =
            setToList $ horrorInvestigators <> damageInvestigators
        for allInvestigators $ \i -> do
          let target = InvestigatorTarget i
          mHealHorror <-
            if i `member` horrorInvestigators
              then getHealHorrorMessage attrs 1 i
              else pure Nothing
          pure
            $ targetLabel
              i
              [ chooseOneAtATime player
                  $ [ componentLabel
                      DamageToken
                      target
                      [HealDamage target (toSource attrs) 1]
                    | i `member` damageInvestigators
                    ]
                  <> [ componentLabel HorrorToken target [healHorror]
                     | healHorror <- maybeToList mHealHorror
                     ]
              ]

      push $ chooseOne player $ assetChoices <> investigatorChoices
      pure a
    _ -> FirstAid3 <$> runMessage msg attrs
