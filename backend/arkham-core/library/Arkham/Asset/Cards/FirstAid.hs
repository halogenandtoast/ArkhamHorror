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
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher

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
          (ControlsThis <> InvestigatorExists
            (AnyInvestigator
              [ HealableInvestigator (toSource x) HorrorType
                $ InvestigatorAt YourLocation
              , HealableInvestigator (toSource x) DamageType
                $ InvestigatorAt YourLocation
              ]
            )
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    InDiscard _ msg'@(UseCardAbility _ source 1 _ _) | isSource attrs source ->
      runMessage msg' a
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
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
        componentLabel component iid' =
          ComponentLabel (InvestigatorComponent iid' component)
      choices <- for allInvestigators $ \i -> do
        mHealHorror <- if i `member` horrorInvestigators
          then getHealHorrorMessage attrs 1 i
          else pure Nothing
        pure $ targetLabel
          i
          [ chooseOrRunOne iid
            $ [ componentLabel
                  DamageToken
                  i
                  [HealDamage (InvestigatorTarget i) (toSource attrs) 1]
              | i `member` damageInvestigators
              ]
            <> [ componentLabel HorrorToken i [healHorror]
               | healHorror <- maybeToList mHealHorror
               ]
          ]
      push $ chooseOne iid choices
      pure a
    _ -> FirstAid <$> runMessage msg attrs
