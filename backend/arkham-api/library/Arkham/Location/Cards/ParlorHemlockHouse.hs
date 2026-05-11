module Arkham.Location.Cards.ParlorHemlockHouse (parlorHemlockHouse) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ParlorHemlockHouse = ParlorHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorHemlockHouse :: LocationCard ParlorHemlockHouse
parlorHemlockHouse =
  locationWith ParlorHemlockHouse Cards.parlorHemlockHouse 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities ParlorHemlockHouse where
  getAbilities (ParlorHemlockHouse a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists targetEnemy)
      $ actionAbilityWithCost (clueCost 1)
   where
    targetEnemy = EnemyAt (ConnectedFrom NotForMovement (be a))

instance RunMessage ParlorHemlockHouse where
  runMessage msg l@(ParlorHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt $ ConnectedFrom NotForMovement (be attrs)
      chooseTargetM iid enemies \enemy -> do
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 enemy
        chooseOneM iid do
          labeled "Test {intellect} (4) for 1 additional damage" do
            sid <- getRandom
            beginSkillTest sid iid (attrs.ability 1) enemy #intellect (Fixed 4)
          labeled "Skip" nothing
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- The skill test target is the chosen enemy; deal the bonus damage to it.
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
        _ -> pure ()
      pure l
    _ -> ParlorHemlockHouse <$> liftRunMessage msg attrs
