module Arkham.Event.Cards.RighteousHunt1 (righteousHunt1, RighteousHunt1 (..)) where

import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection

newtype RighteousHunt1 = RighteousHunt1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

righteousHunt1 :: EventCard RighteousHunt1
righteousHunt1 = event RighteousHunt1 Cards.righteousHunt1

instance RunMessage RighteousHunt1 where
  runMessage msg e@(RighteousHunt1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <-
        select
          $ CanEngageEnemyWithOverride
          $ overrideExists
          $ EnemyAt
          $ LocationWithAccessiblePath (toSource attrs) 2 (InvestigatorWithId iid) Anywhere

      choices <- forMaybeM enemies \enemy -> runMaybeT $ do
        loc <- MaybeT $ field EnemyLocation enemy
        horror <- lift $ field EnemySanityDamage enemy
        n <- lift $ min horror <$> getRemainingBlessTokens
        pure
          $ targetLabel enemy
          $ [Move $ (move attrs iid loc) {moveMeans = OneAtATime}, EnemyEngageInvestigator enemy iid]
          <> replicate n (AddChaosToken #bless)

      player <- getPlayer iid
      push $ chooseOne player choices
      pure e
    _ -> RighteousHunt1 <$> runMessage msg attrs
