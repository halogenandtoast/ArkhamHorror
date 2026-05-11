module Arkham.Treachery.Cards.Fire1 (fire1) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (..))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Fire1 = Fire1 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fire1 :: TreacheryCard Fire1
fire1 = treachery Fire1 Cards.fire1

instance HasAbilities Fire1 where
  getAbilities (Fire1 a) = case a.attached.location of
    Just lid ->
      [ forcedAbility a 1 $ PhaseEnds #when #investigation
      , skillTestAbility $ restricted a 2 (OnLocation $ LocationWithId lid) actionAbility
      ]
    _ -> []

instance RunMessage Fire1 where
  runMessage msg t@(Fire1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      validLocations <-
        select $ NearestLocationTo iid $ not_ $ LocationWithTreachery (treacheryIs Cards.fire1)
      chooseOrRunOneM iid $ targets validLocations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location \lid -> do
        assets <- select $ at_ (LocationWithId lid) <> not_ (AssetWithTrait Elite) <> AssetWithHealth
        enemies <-
          select $ at_ (LocationWithId lid) <> NonEliteEnemy <> EnemyCanBeDamagedBySource (attrs.ability 1)
        investigators <- select $ InvestigatorAt (LocationWithId lid)
        chooseOneAtATimeM iid do
          targets assets \aid -> dealAssetDamage aid (attrs.ability 1) 1
          targets enemies $ nonAttackEnemyDamage Nothing (attrs.ability 1) 1
          targets investigators \iid' -> directDamage iid' (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Fire1 <$> liftRunMessage msg attrs
