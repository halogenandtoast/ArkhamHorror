module Arkham.Asset.Assets.HoracioMartinez (
  horacioMartinez,
  HoracioMartinez(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype HoracioMartinez = HoracioMartinez AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horacioMartinez :: AssetCard HoracioMartinez
horacioMartinez = allyWith HoracioMartinez Cards.horacioMartinez (4, 1) noSlots

instance HasModifiersFor HoracioMartinez where
  getModifiersFor (HoracioMartinez a) = case a.controller of
    Nothing -> pure mempty
    Just controller ->
      modifySelect a (not_ (InvestigatorWithId controller) <> at_ (locationWithAsset a)) [CanAssignDamageToAsset a.id]

instance HasAbilities HoracioMartinez where
  getAbilities (HoracioMartinez a) =
    [ reaction a 1 ControlsThis (exhaust a)
        $ EnemyAttacks #after You AnyEnemyAttack NonEliteEnemy
    ]

instance RunMessage HoracioMartinez where
  runMessage msg a@(HoracioMartinez attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      exhaust (attack.enemy)
      roundModifier (attrs.ability 1) attack.enemy CannotReady
      pure a
    _ -> HoracioMartinez <$> liftRunMessage msg attrs
