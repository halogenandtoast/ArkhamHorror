module Arkham.Location.Cards.MysteriousStairs_185 (
  mysteriousStairs_185,
  MysteriousStairs_185 (..),
)
where

import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Ghoul))

newtype MysteriousStairs_185 = MysteriousStairs_185 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_185 :: LocationCard MysteriousStairs_185
mysteriousStairs_185 =
  locationWith
    MysteriousStairs_185
    Cards.mysteriousStairs_185
    3
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasModifiersFor MysteriousStairs_185 where
  getModifiersFor (InvestigatorTarget iid) (MysteriousStairs_185 attrs) = do
    here <- iid `isAt` attrs
    hasEnemies <- selectAny $ enemyAt attrs <> ReadyEnemy
    pure
      $ toModifiers attrs
      $ guard (here && hasEnemies)
      *> [CannotTakeAction #move, CannotTakeAction #resign]
  getModifiersFor _ _ = pure []

instance HasAbilities MysteriousStairs_185 where
  getAbilities (MysteriousStairs_185 attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)]

instance RunMessage MysteriousStairs_185 where
  runMessage msg l@(MysteriousStairs_185 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ findEncounterCard iid attrs [FromEncounterDeck, FromEncounterDiscard] Ghoul
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      pushM $ createEnemyAt_ card (toId attrs) Nothing
      pure l
    _ -> MysteriousStairs_185 <$> runMessage msg attrs
