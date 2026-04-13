module Arkham.Asset.Assets.HypnoticTherapy (hypnoticTherapy) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (healAdditional)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HypnoticTherapy = HypnoticTherapy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticTherapy :: AssetCard HypnoticTherapy
hypnoticTherapy = asset HypnoticTherapy Cards.hypnoticTherapy

instance HasAbilities HypnoticTherapy where
  getAbilities (HypnoticTherapy a) =
    [ skillTestAbility $ controlled_ a 1 $ actionAbilityWithCost (exhaust a)
    , controlled_ a 2
        $ triggered
          ( InvestigatorHealed #after #horror (affectsOthers Anyone)
              $ SourceOwnedBy You
              <> NotSource (SourceIs (toSource a))
          )
          (exhaust a)
    ]

instance RunMessage HypnoticTherapy where
  runMessage msg a@(HypnoticTherapy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      iids <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
      chooseOrRunOneM iid do
        targets iids \i -> do
          healHorrorIfCan i (attrs.ability 1) 1
          chooseOneM i do
            labeled "Do not draw" nothing
            deckLabeled i $ drawCards i (attrs.ability 1) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws' _ -> do
      healAdditional (attrs.ability 2) #horror ws' 1
      pure a
    _ -> HypnoticTherapy <$> liftRunMessage msg attrs
