module Arkham.Homebrew.DarkMatter.Assets.ShieldingDevice (shieldingDevice) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype ShieldingDevice = ShieldingDevice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shieldingDevice :: AssetCard ShieldingDevice
shieldingDevice = asset ShieldingDevice Cards.shieldingDevice

{- | "[reaction] When an investigator at your location takes any amount of damage
and/or horror, spend 1 resource and exhaust Shielding Device: Cancel that damage
and/or horror."
-}
instance HasAbilities ShieldingDevice where
  getAbilities (ShieldingDevice a) =
    [ controlled a 1 ControlsThis
        $ triggered
          ( oneOf
              [ InvestigatorWouldTakeDamage #when (at_ YourLocation) AnySource AnyDamageType
              , InvestigatorWouldTakeHorror #when (at_ YourLocation) AnySource
              ]
          )
          (exhaust a <> ResourceCost 1)
    ]

instance RunMessage ShieldingDevice where
  runMessage msg a@(ShieldingDevice attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \bearer -> putCardIntoPlay bearer attrs
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeDamage _ (InvestigatorTarget iid') n _ -> push $ CancelDamage iid' n
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n -> push $ CancelHorror iid' n
        _ -> pure ()
      pure a
    _ -> ShieldingDevice <$> liftRunMessage msg attrs
