module Arkham.Location.Cards.ProtoplasmicPool (protoplasmicPool) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype ProtoplasmicPool = ProtoplasmicPool LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protoplasmicPool :: LocationCard ProtoplasmicPool
protoplasmicPool = location ProtoplasmicPool Cards.protoplasmicPool 4 (PerPlayer 1)

instance HasAbilities ProtoplasmicPool where
  getAbilities (ProtoplasmicPool a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (Here <> youExist (InvestigatorWithDormantSeal SealB)) actionAbility

instance RunMessage ProtoplasmicPool where
  runMessage msg l@(ProtoplasmicPool attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      targetAmount <- perPlayer 1
      iids <- select $ investigatorAt attrs
      enemies <- select $ EnemyCanBeDamagedBySource (attrs.ability 1)
      chooseOneM iid do
        labeled "Spend 1 {perPlayer} clues as a group to activate the seal" do
          push $ SpendClues targetAmount iids
          activateSeal SealB
          chooseOneAtATimeM iid $ targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2
        labeled "Do not spend clues" nothing

      pure l
    _ -> ProtoplasmicPool <$> liftRunMessage msg attrs
