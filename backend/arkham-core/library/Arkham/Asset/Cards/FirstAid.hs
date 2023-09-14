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
    [ withCriteria (mkAbility x 1 (actionAbilityWithCost $ assetUseCost x Supply 1))
        $ ControlsThis
          <> InvestigatorExists
            ( AnyInvestigator
                [ HealableInvestigator (toSource x) hType $ InvestigatorAt YourLocation
                | hType <- [HorrorType, DamageType]
                ]
            )
    ]

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    InDiscard _ msg'@(UseThisAbility _ (isSource attrs -> True) 1) -> runMessage msg' a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      horrorInvestigators <- select $ HealableInvestigator source HorrorType $ colocatedWith iid
      damageInvestigators <- select $ HealableInvestigator source DamageType $ colocatedWith iid
      let allInvestigators = toList $ horrorInvestigators <> damageInvestigators
      choices <- for allInvestigators $ \i -> do
        mHealHorror <-
          if i `member` horrorInvestigators then getHealHorrorMessage source 1 i else pure Nothing
        pure
          $ targetLabel i
          $ [ chooseOrRunOne iid
                $ [DamageLabel i [HealDamage (toTarget i) source 1] | i `member` damageInvestigators]
                  <> [HorrorLabel i [healHorror] | healHorror <- toList mHealHorror]
            ]
      push $ chooseOne iid choices
      pure a
    _ -> FirstAid <$> runMessage msg attrs
