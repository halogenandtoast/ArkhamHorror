module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.ErwinBridgeDawn (erwinBridgeDawn) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype ErwinBridgeDawn = ErwinBridgeDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinBridgeDawn :: LocationCard ErwinBridgeDawn
erwinBridgeDawn = symbolLabel $ location ErwinBridgeDawn Cards.erwinBridgeDawn 1 (Static 0)

instance HasAbilities ErwinBridgeDawn where
  getAbilities (ErwinBridgeDawn a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (oneOf [WhileAttacking, WhileEvading]) (FailureResult AnyValue)

instance RunMessage ErwinBridgeDawn where
  runMessage msg l@(ErwinBridgeDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      selectEach (enemyEngagedWith iid) $ push . DisengageEnemy iid
      selectOne (locationIs Cards.unvisitedIsleDawn) >>= traverse_ (moveTo (attrs.ability 1) iid)
      pure l
    _ -> ErwinBridgeDawn <$> liftRunMessage msg attrs
