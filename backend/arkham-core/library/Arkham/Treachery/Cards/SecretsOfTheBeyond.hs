module Arkham.Treachery.Cards.SecretsOfTheBeyond (
  secretsOfTheBeyond,
  SecretsOfTheBeyond (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SecretsOfTheBeyond = SecretsOfTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

secretsOfTheBeyond :: TreacheryCard SecretsOfTheBeyond
secretsOfTheBeyond = treachery SecretsOfTheBeyond Cards.secretsOfTheBeyond

instance RunMessage SecretsOfTheBeyond where
  runMessage msg t@(SecretsOfTheBeyond attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        selectWithField EnemyLocation $ EnemyWithMostDoom $ EnemyWithAnyDoom <> EnemyWithTrait Cultist
      case nonEmpty enemies of
        Just ((enemyId, _) :| _) -> do
          maxDoom <- field EnemyDoom enemyId
          player <- getPlayer iid
          push
            $ chooseOrRunOne
              player
              [targetLabel enemy [PlaceBreaches (toTarget location) maxDoom] | (enemy, Just location) <- enemies]
        Nothing -> push $ gainSurge attrs
      pure t
    _ -> SecretsOfTheBeyond <$> runMessage msg attrs
