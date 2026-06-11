module Arkham.Enemy.Cards.Eixodolon (eixodolon) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype Eixodolon = Eixodolon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eixodolon :: EnemyCard Eixodolon
eixodolon = enemy Eixodolon Cards.eixodolon (2, Static 6, 3) (2, 2)

harvestedPainInVictory :: ExtendedCardMatcher
harvestedPainInVictory = basic $ cardIs Treacheries.harvestedPain

-- Eixodolon gets +6 [per_investigator] health and cannot be defeated by
-- damage. While Harvested Pain is in the victory display, Eixodolon gets +1
-- fight (per copy).
instance HasModifiersFor Eixodolon where
  getModifiersFor (Eixodolon a) = do
    n <- getPlayerCount
    pain <- selectCount $ VictoryDisplayCardMatch harvestedPainInVictory
    modifySelf a $ [HealthModifier (6 * n), CannotBeDefeated] <> [EnemyFight pain | pain > 0]

instance HasAbilities Eixodolon where
  getAbilities (Eixodolon a) =
    extend1 a
      $ restricted a 1 (thisExists a ExhaustedEnemy <> exists (VictoryDisplayCardMatch harvestedPainInVictory))
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage Eixodolon where
  runMessage msg e@(Eixodolon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      pure e
    _ -> Eixodolon <$> liftRunMessage msg attrs
