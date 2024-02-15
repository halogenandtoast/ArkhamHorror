module Arkham.Asset.Cards.MedicalStudent (
  medicalStudent,
  MedicalStudent (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype MedicalStudent = MedicalStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalStudent :: AssetCard MedicalStudent
medicalStudent = ally MedicalStudent Cards.medicalStudent (1, 1)

instance HasAbilities MedicalStudent where
  getAbilities (MedicalStudent x) =
    [ controlledAbility
        x
        1
        ( AnyCriterion
            [ exists
                $ oneOf
                  [ HealableInvestigator (toSource x) #horror $ InvestigatorAt YourLocation
                  , HealableInvestigator (toSource x) #damage $ InvestigatorAt YourLocation
                  ]
            , exists
                $ oneOf
                  [ HealableAsset (toSource x) #horror
                      $ AssetAt YourLocation
                      <> AssetControlledBy (affectsOthers Anyone)
                  , HealableAsset (toSource x) #damage
                      $ AssetAt YourLocation
                      <> AssetControlledBy (affectsOthers Anyone)
                  ]
            ]
        )
        $ freeReaction (AssetEntersPlay #when $ AssetWithId (toId x))
    ]

instance RunMessage MedicalStudent where
  runMessage msg a@(MedicalStudent attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      -- [ALERT] [SAME] FirstAid3
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
            $ HealableAsset (toSource attrs) #horror
            $ AssetAt (locationWithInvestigator iid)
            <> AssetControlledBy (affectsOthers Anyone)
        damageAssets <-
          select
            $ HealableAsset (toSource attrs) #damage
            $ AssetAt (locationWithInvestigator iid)
            <> AssetControlledBy (affectsOthers Anyone)
        let allAssets = horrorAssets <> damageAssets
        pure $ flip map allAssets $ \asset' ->
          let target = AssetTarget asset'
           in targetLabel
                asset'
                [ chooseOneAtATime player
                    $ [ componentLabel
                        DamageToken
                        target
                        [HealDamage target (toSource attrs) 1]
                      | asset' `elem` damageAssets
                      ]
                    <> [ componentLabel
                        HorrorToken
                        target
                        [HealHorror target (toSource attrs) 1]
                       | asset' `elem` horrorAssets
                       ]
                ]

      investigatorChoices <- do
        horrorInvestigators <-
          select
            $ HealableInvestigator (toSource attrs) #horror
            $ colocatedWith iid
        damageInvestigators <-
          select
            $ HealableInvestigator (toSource attrs) #damage
            $ colocatedWith iid
        let
          allInvestigators =
            horrorInvestigators <> damageInvestigators
        for allInvestigators $ \i -> do
          let target = InvestigatorTarget i
          mHealHorror <-
            if i `elem` horrorInvestigators
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
                    | i `elem` damageInvestigators
                    ]
                  <> [ componentLabel HorrorToken target [healHorror]
                     | healHorror <- maybeToList mHealHorror
                     ]
              ]

      push $ chooseOne player $ assetChoices <> investigatorChoices
      pure a
    _ -> MedicalStudent <$> runMessage msg attrs
