module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.DancingRats_040b (dancingRats_040b) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DancingRats_040b = DancingRats_040b EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dancingRats_040b :: EnemyCard DancingRats_040b
dancingRats_040b =
  enemy DancingRats_040b Cards.dancingRats_040b & setSpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor DancingRats_040b where
  -- "While a [[String]] or [[Piano]] treachery is in play, Dancing Rats loses hunter and gains aloof."
  getModifiersFor (DancingRats_040b a) = do
    quiet <-
      selectAny
        $ oneOf
          [TreacheryWithTrait T.String <> InPlayTreachery, TreacheryWithTrait T.Piano <> InPlayTreachery]
    modifySelfWhen a quiet [RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance RunMessage DancingRats_040b where
  runMessage msg (DancingRats_040b attrs) = runQueueT $ DancingRats_040b <$> liftRunMessage msg attrs
