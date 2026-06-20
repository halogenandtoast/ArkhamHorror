module Arkham.Enemy.Cards.PrimevalTerror (primevalTerror) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword (Keyword (Patrol))
import Arkham.Matcher
import Arkham.Trait (Trait (Summit))

newtype PrimevalTerror = PrimevalTerror EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primevalTerror :: EnemyCard PrimevalTerror
primevalTerror = enemy PrimevalTerror Cards.primevalTerror

instance HasModifiersFor PrimevalTerror where
  getModifiersFor (PrimevalTerror a) =
    modifySelf
      a
      [ CannotMakeAttacksOfOpportunity
      , AddKeyword (Patrol $ NearestLocationToYou $ LocationWithTrait Summit <> LocationWithoutInvestigators)
      ]

instance RunMessage PrimevalTerror where
  runMessage msg (PrimevalTerror attrs) = runQueueT $ PrimevalTerror <$> liftRunMessage msg attrs

-- TODO: "Open sky" entry rule — when Primeval Terror would enter an "open sky"
-- card drawn from the Summit deck, and the defeat-at-open-sky resolution. These
-- depend on the unimplemented Summit-deck / open-sky / sliding-location infra.
