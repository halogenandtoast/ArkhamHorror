module Arkham.Types.Enemy.Cards.DiscipleOfTheDevourer
  ( discipleOfTheDevourer
  , DiscipleOfTheDevourer(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.AgendaId
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discipleOfTheDevourer :: EnemyCard DiscipleOfTheDevourer
discipleOfTheDevourer = enemyWith
  DiscipleOfTheDevourer
  Cards.discipleOfTheDevourer
  (3, Static 1, 1)
  (1, 0)
  (spawnAtL ?~ FarthestLocationFromYou EmptyLocation)

instance HasModifiersFor env DiscipleOfTheDevourer

instance ActionRunner env => HasAbilities env DiscipleOfTheDevourer where
  getAbilities i window (DiscipleOfTheDevourer attrs) =
    getAbilities i window attrs

instance EnemyRunner env => RunMessage env DiscipleOfTheDevourer where
  runMessage msg (DiscipleOfTheDevourer attrs) = case msg of
    EnemySpawn (Just iid) _ eid | eid == enemyId attrs -> do
      let
        messages =
          [PlaceDoom (toTarget attrs) 1, InvestigatorPlaceCluesOnLocation iid 1]
      step <- unAgendaStep <$> getStep ()
      if step == 1 then push (chooseOne iid messages) else pushAll messages
      DiscipleOfTheDevourer <$> runMessage msg attrs
    _ -> DiscipleOfTheDevourer <$> runMessage msg attrs
