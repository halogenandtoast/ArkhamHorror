module Arkham.Types.Enemy.Cards.DiscipleOfTheDevourer where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.AgendaId
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discipleOfTheDevourer :: EnemyCard DiscipleOfTheDevourer
discipleOfTheDevourer = enemy DiscipleOfTheDevourer Cards.discipleOfTheDevourer
  $ (healthDamageL .~ 1)
  . (fightL .~ 3)
  . (healthL .~ Static 1)
  . (evadeL .~ 1)
  . (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance HasModifiersFor env DiscipleOfTheDevourer where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DiscipleOfTheDevourer where
  getActions i window (DiscipleOfTheDevourer attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env DiscipleOfTheDevourer where
  runMessage msg (DiscipleOfTheDevourer attrs) = case msg of
    EnemySpawn (Just iid) _ eid | eid == enemyId attrs -> do
      let
        messages =
          [PlaceDoom (toTarget attrs) 1, InvestigatorPlaceCluesOnLocation iid 1]
      step <- unAgendaStep <$> getStep
      if step == 1
        then unshiftMessage (chooseOne iid messages)
        else unshiftMessages messages
      DiscipleOfTheDevourer <$> runMessage msg attrs
    _ -> DiscipleOfTheDevourer <$> runMessage msg attrs
