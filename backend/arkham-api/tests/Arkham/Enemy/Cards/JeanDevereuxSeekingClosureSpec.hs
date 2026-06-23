module Arkham.Enemy.Cards.JeanDevereuxSeekingClosureSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import TestImport.New

-- Laid to Rest (parallel Jim Culver): Jean Devereux's parley makes Jim *draw* the
-- chosen Heretic. Drawing is not a defeat, so the shared Heretic Flip handler must
-- take the draw path: silently remove the enemy from the game (no enemy-defeat
-- windows) and resolve its Unfinished Business back side into the drawer's threat
-- area. The flip's source being something other than the Heretic itself (here,
-- Jean) is what selects that path over the Wages of Sin defeat path.
spec :: Spec
spec = describe "Jean Devereux (\"Seeking Closure\")" do
  it "draws the chosen Heretic: removes it from game and resolves Unfinished Business into the threat area"
    $ gameTest
    $ \investigator -> do
      location <- testLocationWith id
      run $ placedLocation location
      moveTo investigator location

      heretic <- testEnemyWithDef Enemies.heretic_A id
      run $ PlaceEnemy (toId heretic) (AtLocation (toId location))

      -- Flip the Heretic from a non-Heretic source (stands in for Jean's parley).
      -- TestSource is distinct from the Heretic's own EnemySource, so the draw
      -- branch is selected.
      run $ Flip (toId investigator) (TestSource mempty) (toTarget heretic)

      -- Reading the Unfinished Business story presents a single confirm option.
      chooseOnlyOption "resolve Unfinished Business"

      -- The Heretic enemy is gone from play (silently removed; no longer attached
      -- to anything, so it cannot count toward The Beyond's spirit threshold).
      assertNone (InPlayEnemy $ enemyIs Enemies.heretic_A)

      -- Its Unfinished Business back side resolved into the investigator's threat area.
      assert
        $ selectAny
        $ StoryWithTitle "Unfinished Business"
        <> StoryWithPlacement (InThreatArea (toId investigator))
