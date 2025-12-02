module Arkham.Location.Cards.CatacombsOfKomElShoqafaBloodyNexus (catacombsOfKomElShoqafaBloodyNexus) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CatacombsOfKomElShoqafaBloodyNexus = CatacombsOfKomElShoqafaBloodyNexus LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaBloodyNexus :: LocationCard CatacombsOfKomElShoqafaBloodyNexus
catacombsOfKomElShoqafaBloodyNexus =
  symbolLabel
    $ location CatacombsOfKomElShoqafaBloodyNexus Cards.catacombsOfKomElShoqafaBloodyNexus 5 (PerPlayer 2)

instance HasModifiersFor CatacombsOfKomElShoqafaBloodyNexus where
  getModifiersFor (CatacombsOfKomElShoqafaBloodyNexus a) = do
    blockedWhenUnrevealed a
    modifySelectWhen
      a
      a.unrevealed
      (enemyIs Enemies.theBeastInACowlOfCrimsonRavagerInRed)
      [CannotMove, CannotBeMoved]

instance HasAbilities CatacombsOfKomElShoqafaBloodyNexus where
  getAbilities (CatacombsOfKomElShoqafaBloodyNexus a) =
    withBaseAbilities a
      $ [restricted a 1 (exists $ AgendaWithDoom $ atLeast 6) $ forced AnyWindow | a.unrevealed]
      <> [ skillTestAbility
             $ groupLimit PerGame
             $ restricted a 1 (Here <> exists (enemyIs Enemies.theBeastInACowlOfCrimsonRavagerInRed)) actionAbility
         | a.revealed
         ]

instance RunMessage CatacombsOfKomElShoqafaBloodyNexus where
  runMessage msg l@(CatacombsOfKomElShoqafaBloodyNexus attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 | attrs.unrevealed -> do
      reveal attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      selectEach (enemyIs Enemies.theBeastInACowlOfCrimsonRavagerInRed) (automaticallyEvadeEnemy iid)
      pure l
    _ -> CatacombsOfKomElShoqafaBloodyNexus <$> liftRunMessage msg attrs
