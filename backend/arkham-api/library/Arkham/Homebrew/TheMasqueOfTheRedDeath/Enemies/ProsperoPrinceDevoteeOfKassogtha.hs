module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.ProsperoPrinceDevoteeOfKassogtha (
  prosperoPrinceDevoteeOfKassogtha,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhenM)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype ProsperoPrinceDevoteeOfKassogtha = ProsperoPrinceDevoteeOfKassogtha EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

prosperoPrinceDevoteeOfKassogtha :: EnemyCard ProsperoPrinceDevoteeOfKassogtha
prosperoPrinceDevoteeOfKassogtha = enemy ProsperoPrinceDevoteeOfKassogtha Cards.prosperoPrinceDevoteeOfKassogtha

instance HasModifiersFor ProsperoPrinceDevoteeOfKassogtha where
  getModifiersFor (ProsperoPrinceDevoteeOfKassogtha a) = do
    -- "While Prospero Prince is ready, investigators cannot resign."
    modifySelect a Anyone [CannotTakeAction (IsAction #resign) | a.ready]
    -- "While The Red Death is at Prospero Prince's location, Prospero Prince gets
    -- -2 fight and cannot attack."
    modifySelfWhenM
      a
      (selectAny $ enemyIs Cards.theRedDeath <> EnemyAt (locationWithEnemy a.id))
      [EnemyFight (-2), CannotAttack]

instance RunMessage ProsperoPrinceDevoteeOfKassogtha where
  runMessage msg (ProsperoPrinceDevoteeOfKassogtha attrs) =
    runQueueT $ ProsperoPrinceDevoteeOfKassogtha <$> liftRunMessage msg attrs
