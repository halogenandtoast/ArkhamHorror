module Arkham.Asset.Cards.DrCharlesWestIIIKnowsHisPurpose (
  drCharlesWestIiiKnowsHisPurpose,
  DrCharlesWestIIIKnowsHisPurpose (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Slot
import Arkham.Trait

newtype DrCharlesWestIIIKnowsHisPurpose = DrCharlesWestIIIKnowsHisPurpose AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drCharlesWestIiiKnowsHisPurpose :: AssetCard DrCharlesWestIIIKnowsHisPurpose
drCharlesWestIiiKnowsHisPurpose = ally DrCharlesWestIIIKnowsHisPurpose Cards.drCharlesWestIiiKnowsHisPurpose (1, 2)

instance HasAbilities DrCharlesWestIIIKnowsHisPurpose where
  getAbilities (DrCharlesWestIIIKnowsHisPurpose a) =
    [ controlledAbility a 1 (exists $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (a.ability 1))
        $ ReactionAbility
          ( SkillTestResult #after You #investigating
              $ oneOf [SuccessResult (static 1), SuccessResult (static 3)]
          )
          (exhaust a)
    ]

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tool []

instance RunMessage DrCharlesWestIIIKnowsHisPurpose where
  runMessage msg a@(DrCharlesWestIIIKnowsHisPurpose attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid #hand (slot attrs)
      DrCharlesWestIIIKnowsHisPurpose <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1)
        $ enemyAtLocationWith iid
        <> EnemyCanBeDamagedBySource (attrs.ability 1)
      pure a
    HandleTargetChoice _iid (isSource attrs -> True) (EnemyTarget eid) -> do
      nonAttackEnemyDamage (attrs.ability 1) 1 eid
      pure a
    _ -> DrCharlesWestIIIKnowsHisPurpose <$> liftRunMessage msg attrs
