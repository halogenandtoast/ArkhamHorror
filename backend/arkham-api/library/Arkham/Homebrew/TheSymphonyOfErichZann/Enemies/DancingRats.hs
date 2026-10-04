module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.DancingRats (dancingRats) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Treacheries
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DancingRats = DancingRats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dancingRats :: EnemyCard DancingRats
dancingRats = enemy DancingRats Cards.dancingRats & setSpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor DancingRats where
  -- While Dies Irae is in play it stops hunting and turns aloof instead.
  getModifiersFor (DancingRats a) = do
    diesIrae <- selectAny $ treacheryIs Treacheries.diesIrae <> InPlayTreachery
    modifySelfWhen a diesIrae [RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance RunMessage DancingRats where
  runMessage msg (DancingRats attrs) = runQueueT $ DancingRats <$> liftRunMessage msg attrs
