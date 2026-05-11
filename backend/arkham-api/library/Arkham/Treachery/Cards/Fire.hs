module Arkham.Treachery.Cards.Fire (fire) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Fire = Fire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fire :: TreacheryCard Fire
fire = treachery Fire Cards.fire

instance HasAbilities Fire where
  getAbilities (Fire a) = case a.attached.location of
    Just lid ->
      [ forcedAbility a 1 $ RoundEnds #when
      , skillTestAbility $ restricted a 2 (OnLocation $ LocationWithId lid) actionAbility
      ]
    _ -> []

instance RunMessage Fire where
  runMessage msg t@(Fire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let noFire = not_ (LocationWithTreachery (treacheryIs Cards.fire))
      validLocations <-
        select
          $ NearestLocationTo iid
          $ noFire <> ConnectedTo ForMovement (LocationWithTreachery (treacheryIs Cards.fire))
      finalLocations <-
        if null validLocations
          then select $ NearestLocationTo iid noFire
          else pure validLocations
      chooseOrRunOneM iid $ targets finalLocations $ attachTreachery attrs
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location \lid -> do
        assets <- select $ at_ (LocationWithId lid) <> AssetWithHealth
        enemies <- select $ at_ (LocationWithId lid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
        investigators <- select $ InvestigatorAt (LocationWithId lid)
        for_ assets \aid -> dealAssetDirectDamage aid (attrs.ability 1) 1
        for_ enemies $ nonAttackEnemyDamage Nothing (attrs.ability 1) 1
        for_ investigators \iid' -> directDamage iid' (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Fire <$> liftRunMessage msg attrs
