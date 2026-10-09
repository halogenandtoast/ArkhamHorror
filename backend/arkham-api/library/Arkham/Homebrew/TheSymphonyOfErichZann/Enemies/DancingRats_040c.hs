module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.DancingRats_040c (dancingRats_040c) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DancingRats_040c = DancingRats_040c EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dancingRats_040c :: EnemyCard DancingRats_040c
dancingRats_040c =
  enemy DancingRats_040c Cards.dancingRats_040c & setSpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor DancingRats_040c where
  -- "While a [[Brass]] or [[Percussion]] treachery is in play, Dancing Rats loses hunter and gains aloof."
  getModifiersFor (DancingRats_040c a) = do
    quiet <-
      selectAny
        $ oneOf
          [TreacheryWithTrait Brass <> InPlayTreachery, TreacheryWithTrait Percussion <> InPlayTreachery]
    modifySelfWhen a quiet [RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance RunMessage DancingRats_040c where
  runMessage msg (DancingRats_040c attrs) = runQueueT $ DancingRats_040c <$> liftRunMessage msg attrs
