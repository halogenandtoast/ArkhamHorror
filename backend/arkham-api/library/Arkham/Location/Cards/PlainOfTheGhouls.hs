module Arkham.Location.Cards.PlainOfTheGhouls (plainOfTheGhouls, PlainOfTheGhouls (..)) where

import Arkham.Enemy.Types (Field (EnemyTraits))
import Arkham.Game.Helpers (perPlayer)
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait (Gug))

newtype PlainOfTheGhouls = PlainOfTheGhouls LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plainOfTheGhouls :: LocationCard PlainOfTheGhouls
plainOfTheGhouls = location PlainOfTheGhouls Cards.plainOfTheGhouls 4 (PerPlayer 1)

instance HasModifiersFor PlainOfTheGhouls where
  getModifiersFor (EnemyTarget eid) (PlainOfTheGhouls attrs) = do
    isGug <- fieldMap EnemyTraits (member Gug) eid
    pure
      $ toModifiers attrs
      $ guard isGug
      *> [CannotEnter attrs.id, ChangeSpawnLocation (be attrs) (locationIs Cards.cityOfGugs)]
  getModifiersFor _ _ = pure []

instance HasAbilities PlainOfTheGhouls where
  getAbilities (PlainOfTheGhouls attrs) = veiled attrs []

instance RunMessage PlainOfTheGhouls where
  runMessage msg (PlainOfTheGhouls attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.aStrangeGhoul
      clues <- selectSum InvestigatorClues UneliminatedInvestigator
      n <- perPlayer 3
      pure . PlainOfTheGhouls $ attrs & canBeFlippedL .~ (clues < n)
    _ -> PlainOfTheGhouls <$> runMessage msg attrs
