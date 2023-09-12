module Arkham.Asset.Cards.FirstAid (
  FirstAid (..),
  firstAid,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
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
    [ withCriteria (mkAbility x 1 (ActionAbility Nothing $ ActionCost 1 <> assetUseCost x Supply 1))
        $ ControlsThis
          <> InvestigatorExists
            ( AnyInvestigator
                [ HealableInvestigator (toSource x) HorrorType $ InvestigatorAt YourLocation
                , HealableInvestigator (toSource x) DamageType $ InvestigatorAt YourLocation
                ]
            )
    ]

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    InDiscard _ msg'@(UseCardAbility _ (isSource attrs -> True) 1 _ _) -> runMessage msg' a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      horrorInvestigators <-
        select $ HealableInvestigator (toAbilitySource attrs 1) HorrorType $ colocatedWith iid
      damageInvestigators <-
        select $ HealableInvestigator (toAbilitySource attrs 1) DamageType $ colocatedWith iid
      let
        allInvestigators = setToList $ horrorInvestigators <> damageInvestigators
        componentLabel component iid' = ComponentLabel (InvestigatorComponent iid' component)
      choices <- for allInvestigators $ \i -> do
        mHealHorror <-
          if i `member` horrorInvestigators
            then getHealHorrorMessage (toAbilitySource attrs 1) 1 i
            else pure Nothing
        pure
          $ targetLabel i
          $ [ chooseOrRunOne iid
                $ [ componentLabel DamageToken i [HealDamage (InvestigatorTarget i) (toAbilitySource attrs 1) 1]
                  | i `member` damageInvestigators
                  ]
                  <> [componentLabel HorrorToken i [healHorror] | healHorror <- maybeToList mHealHorror]
            ]
      push $ chooseOne iid choices
      pure a
    _ -> FirstAid <$> runMessage msg attrs
