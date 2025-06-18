module Arkham.Asset.Assets.HoracioMartinez (horacioMartinez) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher

newtype HoracioMartinez = HoracioMartinez AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horacioMartinez :: AssetCard HoracioMartinez
horacioMartinez = allyWith HoracioMartinez Cards.horacioMartinez (4, 1) noSlots

instance HasModifiersFor HoracioMartinez where
  getModifiersFor (HoracioMartinez a) = for_ a.controller \iid ->
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id]

instance HasAbilities HoracioMartinez where
  getAbilities (HoracioMartinez a) =
    [ reaction a 1 ControlsThis (exhaust a)
        $ EnemyAttacks #after You AnyEnemyAttack NonEliteEnemy
    ]

instance RunMessage HoracioMartinez where
  runMessage msg a@(HoracioMartinez attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      exhaustThis attack.enemy
      roundModifier (attrs.ability 1) attack.enemy CannotReady
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ HoracioMartinez $ attrs & flippedL .~ True & visibleL .~ False
    _ -> HoracioMartinez <$> liftRunMessage msg attrs
