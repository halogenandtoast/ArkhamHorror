module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.ErwinBridgeDusk (erwinBridgeDusk) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype ErwinBridgeDusk = ErwinBridgeDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinBridgeDusk :: LocationCard ErwinBridgeDusk
erwinBridgeDusk = symbolLabel $ location ErwinBridgeDusk Cards.erwinBridgeDusk 1 (Static 0)

instance HasAbilities ErwinBridgeDusk where
  getAbilities (ErwinBridgeDusk a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (oneOf [WhileAttacking, WhileEvading]) (FailureResult AnyValue)

instance RunMessage ErwinBridgeDusk where
  runMessage msg l@(ErwinBridgeDusk attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directDamage iid (attrs.ability 1) 1
      selectEach (enemyEngagedWith iid) $ push . DisengageEnemy iid
      selectOne (locationIs Cards.unvisitedIsleDusk) >>= traverse_ (moveTo (attrs.ability 1) iid)
      pure l
    _ -> ErwinBridgeDusk <$> liftRunMessage msg attrs
