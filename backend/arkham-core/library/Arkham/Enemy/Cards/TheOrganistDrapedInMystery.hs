module Arkham.Enemy.Cards.TheOrganistDrapedInMystery (
  theOrganistDrapedInMystery,
  TheOrganistDrapedInMystery (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Helpers
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheOrganistDrapedInMystery = TheOrganistDrapedInMystery EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

instance HasModifiersFor TheOrganistDrapedInMystery where
  getModifiersFor target (TheOrganistDrapedInMystery attrs) | isTarget attrs target = do
    pure $ toModifiers attrs [CannotBeDamaged]
  getModifiersFor _ _ = pure []

instance HasAbilities TheOrganistDrapedInMystery where
  getAbilities (TheOrganistDrapedInMystery attrs) =
    withBaseAbilities attrs [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #after #enemy]

theOrganistDrapedInMystery :: EnemyCard TheOrganistDrapedInMystery
theOrganistDrapedInMystery =
  enemyWith
    TheOrganistDrapedInMystery
    Cards.theOrganistDrapedInMystery
    (3, Static 1, 5)
    (0, 1)
    (healthL .~ Nothing)

instance RunMessage TheOrganistDrapedInMystery where
  runMessage msg e@(TheOrganistDrapedInMystery attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- selectList $ investigatorEngagedWith (toId attrs)
      if null iids
        then push =<< moveOrganistAwayFromNearestInvestigator
        else pushAll [DisengageEnemy iid $ toId attrs | iid <- iids]
      pure e
    _ -> TheOrganistDrapedInMystery <$> runMessage msg attrs
