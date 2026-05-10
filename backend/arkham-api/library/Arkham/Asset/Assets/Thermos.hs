module Arkham.Asset.Assets.Thermos (thermos) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Thermos = Thermos AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thermos :: AssetCard Thermos
thermos = asset Thermos Cards.thermos

instance HasAbilities Thermos where
  getAbilities (Thermos a) =
    [ (cardI18n $ withI18nTooltip "thermos.heal1DamageFromAnInvestigatorAtYourLocation2DamageInsteadIfH")
        $ controlled a 1 (exists $ HealableInvestigator (toSource a) #damage $ colocatedWithMatch You)
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Supply 1)
    , (cardI18n $ withI18nTooltip "thermos.heal1HorrorFromAnInvestigatorAtYourLocation2HorrorInsteadIfH")
        $ controlled a 2 (exists $ HealableInvestigator (toSource a) #horror $ colocatedWithMatch You)
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage Thermos where
  runMessage msg a@(Thermos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid
      chooseOrRunOneM iid $ targets investigators $ handleTarget iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid
      chooseOrRunOneM iid $ targets investigators $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      trauma <- field InvestigatorPhysicalTrauma iid'
      healDamageIfCan iid' (attrs.ability 1) $ if trauma >= 2 then 2 else 1
      pure a
    HandleTargetChoice _ (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      trauma <- field InvestigatorMentalTrauma iid'
      healHorrorIfCan iid' (attrs.ability 2) $ if trauma >= 2 then 2 else 1
      pure a
    _ -> Thermos <$> liftRunMessage msg attrs
