module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.DancingRats_040a (dancingRats_040a) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Treacheries
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DancingRats_040a = DancingRats_040a EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dancingRats_040a :: EnemyCard DancingRats_040a
dancingRats_040a =
  enemy DancingRats_040a Cards.dancingRats_040a & setSpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor DancingRats_040a where
  -- "While the Dies Irae treachery is in play, Dancing Rats loses hunter and gains aloof."
  getModifiersFor (DancingRats_040a a) = do
    quiet <- selectAny $ treacheryIs Treacheries.diesIrae <> InPlayTreachery
    modifySelfWhen a quiet [RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance RunMessage DancingRats_040a where
  runMessage msg (DancingRats_040a attrs) = runQueueT $ DancingRats_040a <$> liftRunMessage msg attrs
