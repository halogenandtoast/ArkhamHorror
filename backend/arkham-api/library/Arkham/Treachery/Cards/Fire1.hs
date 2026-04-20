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
      , skillTestAbility $ restrictedAbility a 2 (OnLocation $ LocationWithId lid) 
          $ ActionAbility mempty (Just $ AbilitySkill #agility) $ ActionCost 1
      ]
    _ -> []

instance RunMessage Fire1 where
  runMessage msg t@(Fire1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Find nearest location without Fire! attached
      locationsWithoutFire <- select $ NearestLocationTo iid $ NotLocation 
        $ LocationWithTreachery (treacheryIs Cards.fire1)
      case locationsWithoutFire of
        [] -> pure ()
        [loc] -> attachTreachery attrs loc
        locs -> chooseTargetM iid locs $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Deal 1 direct damage to each non-Elite card with health at this location (including investigators)
      case attrs.attached.location of
        Nothing -> pure ()
        Just lid -> do
          assets <- select $ AssetAt (LocationWithId lid) <> not_ (AssetWithTrait Elite) <> AssetWithHealth
          enemies <- select $ EnemyAt (LocationWithId lid) <> NonEliteEnemy
          investigators <- select $ InvestigatorAt (LocationWithId lid)
          for_ assets $ \aid -> dealAssetDamage aid attrs 1
          for_ enemies $ \eid -> nonAttackEnemyDamage Nothing (toSource attrs) 1 eid
          for_ investigators $ \iid' -> assignDamage iid' (toSource attrs) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (toSource attrs) (toTarget attrs)
      pure t
    _ -> Fire1 <$> liftRunMessage msg attrs
