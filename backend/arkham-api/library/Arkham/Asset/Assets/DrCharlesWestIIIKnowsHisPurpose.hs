module Arkham.Asset.Assets.DrCharlesWestIIIKnowsHisPurpose (drCharlesWestIiiKnowsHisPurpose) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Slot
import Arkham.Trait

newtype DrCharlesWestIIIKnowsHisPurpose = DrCharlesWestIIIKnowsHisPurpose AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drCharlesWestIiiKnowsHisPurpose :: AssetCard DrCharlesWestIIIKnowsHisPurpose
drCharlesWestIiiKnowsHisPurpose = ally DrCharlesWestIIIKnowsHisPurpose Cards.drCharlesWestIiiKnowsHisPurpose (1, 2)

instance HasAbilities DrCharlesWestIIIKnowsHisPurpose where
  getAbilities (DrCharlesWestIIIKnowsHisPurpose a) =
    [ controlled a 1 (canDamageEnemyAt (a.ability 1) YourLocation)
        $ triggered
          ( SkillTestResult #after You #investigating
              $ oneOf [SuccessResult (static 1), SuccessResult (static 3)]
          )
          (exhaust a)
    ]

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tool []

instance RunMessage DrCharlesWestIIIKnowsHisPurpose where
  runMessage msg a@(DrCharlesWestIIIKnowsHisPurpose attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid #hand (slot attrs)
      DrCharlesWestIIIKnowsHisPurpose <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseDamageEnemy iid (attrs.ability 1) (locationWithInvestigator iid) AnyEnemy 1
      pure a
    _ -> DrCharlesWestIIIKnowsHisPurpose <$> liftRunMessage msg attrs
