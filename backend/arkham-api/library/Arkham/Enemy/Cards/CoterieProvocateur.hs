module Arkham.Enemy.Cards.CoterieProvocateur (coterieProvocateur) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers

newtype CoterieProvocateur = CoterieProvocateur EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

coterieProvocateur :: EnemyCard CoterieProvocateur
coterieProvocateur =
  enemy CoterieProvocateur Cards.coterieProvocateur (4, Static 2, 3) (0, 1)
    & setSpawnAt (NearestLocationToYou locationWithKeyLocus)

instance HasModifiersFor CoterieProvocateur where
  getModifiersFor (CoterieProvocateur a) = do
    when a.ready do
      modifySelect
        a
        (InvestigatorAt $ locationWithEnemy a)
        [CannotFight (not_ $ EnemyWithTitle "Coterie Provocateur")]
