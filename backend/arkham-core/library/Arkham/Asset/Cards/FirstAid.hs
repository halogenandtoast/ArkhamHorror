module Arkham.Asset.Cards.FirstAid (
  FirstAid (..),
  firstAid,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = assetWith FirstAid Cards.firstAid discardWhenNoUses

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) =
    [ controlledAbility
        x
        1
        (exists $ oneOf $ map healable [#horror, #damage])
        (actionAbilityWithCost $ assetUseCost x Supply 1)
    ]
   where
    healable hType = HealableInvestigator (toAbilitySource x 1) hType $ InvestigatorAt YourLocation

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      horrorInvestigators <- select $ HealableInvestigator source #horror $ colocatedWith iid
      damageInvestigators <- select $ HealableInvestigator source #damage $ colocatedWith iid
      let allInvestigators = toList $ horrorInvestigators <> damageInvestigators
      player <- getPlayer iid
      choices <- for allInvestigators $ \i -> do
        mHealHorror <-
          runMaybeT $ guard (i `member` horrorInvestigators) *> MaybeT (getHealHorrorMessage source 1 i)
        pure
          $ targetLabel i
          . only
          $ chooseOrRunOne player
          $ [DamageLabel i [HealDamage (toTarget i) source 1] | i `member` damageInvestigators]
          <> [HorrorLabel i [healHorror] | healHorror <- toList mHealHorror]
      pushIfAny choices $ chooseOne player choices
      pure a
    _ -> FirstAid <$> runMessage msg attrs
