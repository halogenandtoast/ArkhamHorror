module Arkham.Asset.Assets.CloakOfResonance (cloakOfResonance) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CloakOfResonance = CloakOfResonance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloakOfResonance :: AssetCard CloakOfResonance
cloakOfResonance = assetWith CloakOfResonance Cards.cloakOfResonance (sanityL ?~ 3)

instance HasAbilities CloakOfResonance where
  getAbilities (CloakOfResonance a) =
    [ controlled a 1 (canDamageEnemyAt (a.ability 1) YourLocation)
        $ triggered (PlacedCounterOnAsset #after (be a) AnySource #horror $ atLeast 1) (exhaust a)
    ]

instance RunMessage CloakOfResonance where
  runMessage msg a@(CloakOfResonance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseDamageEnemy iid (attrs.ability 1) (locationWithInvestigator iid) AnyEnemy 1
      pure a
    _ -> CloakOfResonance <$> liftRunMessage msg attrs
