module Arkham.Story.Cards.TheUnveiling (theUnveiling) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (getDoomAmount, getEnemy)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.DealingsInTheDark.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheUnveiling = TheUnveiling StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnveiling :: StoryCard TheUnveiling
theUnveiling = story TheUnveiling Cards.theUnveiling

instance HasAbilities TheUnveiling where
  getAbilities (TheUnveiling a) =
    [ mkAbility a 1 $ forced $ PlacedCounterOnEnemy #after EnemyWithTrait Cultist AnySource #doom (atLeast 1)
    , restricted a 2 (exists $ EnemyWithClues (moreThan 3)) Anytime
    ]

instance HasModifiersFor TheUnveiling where
  getModifiersFor (TheUnveiling a) = do
    cultistClues <- getCluesPossesedByTheCult
    act <- getCurrentActStep
    n <- perPlayer (4 * act)
    when (cultistClues >= n) do
      modifySelf a [ScenarioModifier "cultHasEnoughClues"]

instance RunMessage TheUnveiling where
  runMessage msg s@(TheUnveiling attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy &&& getDoomAmount -> (eid, n)) _ -> do
      flipDoomToClues eid n
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (EnemyWithClues $ moreThan 3) \enemy -> do
        clues <- field EnemyClues enemy
        moveTokens (attrs.ability 2) enemy attrs #clue (max 0 $ clues - 3)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      removeStory attrs
      theUnsealing <- genCard Cards.theUnsealing
      push $ PlaceStory theUnsealing Global
      pure s
    _ -> TheUnveiling <$> liftRunMessage msg attrs
