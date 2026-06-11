module Arkham.Asset.Assets.NikolaTesla (nikolaTesla) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype NikolaTesla = NikolaTesla AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nikolaTesla :: AssetCard NikolaTesla
nikolaTesla = ally NikolaTesla Cards.nikolaTesla (2, 1)

instance HasAbilities NikolaTesla where
  getAbilities (NikolaTesla a) =
    [restricted a 1 OnSameLocation $ FastAbility' (exhaust a) #parley]

usedOptions :: AssetAttrs -> [Text]
usedOptions attrs = toResultDefault [] attrs.meta

instance RunMessage NikolaTesla where
  runMessage msg a@(NikolaTesla attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let used = usedOptions attrs
      chooseOrRunOneM iid $ withI18n do
        when ("clue" `notElem` used) do
          countVar 1 $ labeled' "gainClues" $ handleTarget iid attrs (LabeledTarget "clue" $ toTarget attrs)
        when ("cards" `notElem` used) do
          countVar 2 $ labeled' "drawCards" $ handleTarget iid attrs (LabeledTarget "cards" $ toTarget attrs)
        when ("resources" `notElem` used) do
          countVar 3 $ labeled' "gainResources" $ handleTarget iid attrs (LabeledTarget "resources" $ toTarget attrs)
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (LabeledTarget label _) -> do
      case label of
        "clue" -> gainClues iid (attrs.ability 1) 1
        "cards" -> drawCards iid (attrs.ability 1) 2
        "resources" -> gainResources iid (attrs.ability 1) 3
        _ -> pure ()
      pure $ NikolaTesla $ attrs & setMeta (label : usedOptions attrs)
    _ -> NikolaTesla <$> liftRunMessage msg attrs
