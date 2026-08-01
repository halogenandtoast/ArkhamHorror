module Arkham.Agenda.Cards.EmptyStreetsSpec (spec) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Entities qualified as Entities
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import TestImport.New

{- | Regression for #5326. Empty Streets reads "Each [Risen] enemy and each
[Ghoul] enemy gains hunter and 'Spawn - Random location.'", but the
implementation applied the random spawn to every enemy and never granted
hunter. That sent The Red-Gloved Man -- who has no printed spawn
instruction -- to a random location instead of the location of the
investigator who drew him.
-}
realAgenda :: CardDef -> TestAppT ()
realAgenda def = do
  card <- genCard def
  let agenda' = lookupAgenda (AgendaId (toCardCode card)) 1 (toCardId card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda'

spec :: Spec
spec = describe "Empty Streets" do
  it "gives Ghoul enemies hunter" . gameTest $ \self -> do
    realAgenda Agendas.emptyStreets
    location <- testLocation
    self `moveTo` location
    ghoulMinion <- testEnemyWithDef Enemies.ghoulMinion id
    ghoulMinion `spawnAt` location
    assertAny $ HunterEnemy <> EnemyWithId (toId ghoulMinion)

  it "does not give hunter to enemies without the Risen or Ghoul trait" . gameTest $ \self -> do
    realAgenda Agendas.emptyStreets
    location <- testLocation
    self `moveTo` location
    yithianObserver <- testEnemyWithDef Enemies.yithianObserver id
    yithianObserver `spawnAt` location
    assertNone $ HunterEnemy <> EnemyWithId (toId yithianObserver)

  it "does not overwrite the spawn location of enemies without the Risen or Ghoul trait"
    . gameTest
    $ \self -> do
      realAgenda Agendas.emptyStreets
      location <- testLocation
      -- Other locations the (buggy) random spawn could have sent the enemy to.
      _ <- testLocation
      _ <- testLocation
      _ <- testLocation
      self `moveTo` location
      -- Draw several so a random spawn cannot coincidentally land every one of
      -- them on the investigator's location.
      for_ [1 :: Int .. 5] \_ -> do
        yithianObserver <- testEnemyWithDef Enemies.yithianObserver id
        -- Let the modifier cache pick the newly inserted enemy up before it is drawn.
        tick
        run $ InvestigatorDrawEnemy (toId self) (toId yithianObserver)
        field EnemyPlacement (toId yithianObserver) `shouldReturn` InThreatArea (toId self)
