module Arkham.Location.Cards.MysteriousStairs_185 (mysteriousStairs_185, MysteriousStairs_185 (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Ghoul))

newtype MysteriousStairs_185 = MysteriousStairs_185 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_185 :: LocationCard MysteriousStairs_185
mysteriousStairs_185 =
  locationWith MysteriousStairs_185 Cards.mysteriousStairs_185 3 (Static 0)
    $ connectsToL
    .~ setFromList [Above, Below]

instance HasModifiersFor MysteriousStairs_185 where
  getModifiersFor (MysteriousStairs_185 a) = whenRevealed a do
    readyEnemy <- selectAny $ enemyAt a <> ReadyEnemy
    if readyEnemy
      then modifySelect a (investigatorAt a) [CannotTakeAction #move, CannotTakeAction #resign]
      else pure mempty

instance HasAbilities MysteriousStairs_185 where
  getAbilities (MysteriousStairs_185 attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)

instance RunMessage MysteriousStairs_185 where
  runMessage msg l@(MysteriousStairs_185 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs Ghoul
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAt_ card attrs.id
      pure l
    _ -> MysteriousStairs_185 <$> liftRunMessage msg attrs
