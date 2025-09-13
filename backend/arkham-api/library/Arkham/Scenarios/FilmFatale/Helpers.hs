module Arkham.Scenarios.FilmFatale.Helpers where

import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Layout
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Move
import Arkham.Prelude
import Arkham.Source

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "filmFatale" a

initialLayout :: [GridTemplateRow]
initialLayout =
  [ "jungleSet spaceSet   gothicSet"
  , ".         centralLot ."
  ]

moveContessa
  :: (ReverseQueue m, Sourceable source, ToId location LocationId) => source -> location -> m ()
moveContessa source location = do
  contessa <- selectJust $ enemyIs Enemies.theContessaNeedlesslySmug
  readyThis contessa
  enemyMoveTo source contessa location
