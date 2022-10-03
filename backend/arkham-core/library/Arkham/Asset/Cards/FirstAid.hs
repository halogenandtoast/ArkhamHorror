module Arkham.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = assetWith FirstAid Cards.firstAid (discardWhenNoUsesL .~ True)


-- validity here is a little complex, you have to be able to heal horror and an investigator exists at your location that has any horror, or the same for damage

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) =
    [ restrictedAbility
          x
          1
          (ControlsThis <> AnyCriterion
            [ InvestigatorExists (You <> InvestigatorCanHealHorror)
              <> InvestigatorExists
                   (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
            , InvestigatorExists (You <> InvestigatorCanHealDamage)
              <> InvestigatorExists
                   (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
            ]
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    InDiscard _ msg'@(UseCardAbility _ source 1 _ _) | isSource attrs source ->
      runMessage msg' a
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      canHealHorror <- iid <=~> InvestigatorCanHealHorror
      canHealDamage <- iid <=~> InvestigatorCanHealDamage
      horrorTargets <- if canHealHorror
        then
          selectListMap InvestigatorTarget
          $ colocatedWith iid
          <> InvestigatorWithAnyHorror
        else pure []
      damageTargets <- if canHealDamage
        then
          selectListMap InvestigatorTarget
          $ colocatedWith iid
          <> InvestigatorWithAnyDamage
        else pure []
      let
        targets = nub $ horrorTargets <> damageTargets
        componentLabel component target = case target of
          InvestigatorTarget iid' ->
            ComponentLabel (InvestigatorComponent iid' component)
          AssetTarget aid -> ComponentLabel (AssetComponent aid component)
          _ -> error "unhandled target"
      push $ chooseOne
        iid
        [ TargetLabel
            target
            [ chooseOne iid
              $ [ componentLabel DamageToken target [HealDamage target 1]
                | target `elem` damageTargets
                ]
              <> [ componentLabel HorrorToken target [HealHorror target 1]
                 | target `elem` horrorTargets
                 ]
            ]
        | target <- targets
        ]
      pure a
    _ -> FirstAid <$> runMessage msg attrs
