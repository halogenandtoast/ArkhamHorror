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
    InDiscard _ msg'@(UseCardAbility _ source _ 1 _) | isSource attrs source ->
      runMessage msg' a
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      canHealHorror <- iid <=~> InvestigatorCanHealHorror
      canHealDamage <- iid <=~> InvestigatorCanHealDamage
      horrorTargets <- if canHealHorror
        then selectList $ colocatedWith iid <> InvestigatorWithAnyHorror
        else pure []
      damageTargets <- if canHealDamage
        then selectList $ colocatedWith iid <> InvestigatorWithAnyDamage
        else pure []
      let iids = nub $ horrorTargets <> damageTargets
      push $ chooseOne
        iid
        [ TargetLabel
            (InvestigatorTarget iid')
            [ chooseOne iid
              $ [ ComponentLabel
                    (InvestigatorComponent iid' DamageToken)
                    [HealDamage (InvestigatorTarget iid') 1]
                | iid' `elem` damageTargets
                ]
              <> [ ComponentLabel
                     (InvestigatorComponent iid' HorrorToken)
                     [HealHorror (InvestigatorTarget iid') 1]
                 | iid' `elem` horrorTargets
                 ]
            ]
        | iid' <- iids
        ]
      pure a
    _ -> FirstAid <$> runMessage msg attrs
