module Arkham.Location.Cards.Coop (coop) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype Coop = Coop LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coop :: LocationCard Coop
coop = symbolLabel $ locationWith Coop Cards.coop 4 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor Coop where
  getModifiersFor (Coop a) = do
    modifySelfWhen a (a.token #horror > 0) [ShroudModifier (-2)]
    modifySelect a (enemyAt a) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities Coop where
  getAbilities (Coop a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage Coop where
  runMessage msg l@(Coop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      fires <- getSetAsideCardsMatching $ cardIs Treacheries.fire
      case fires of
        (drawFire : shuffleFire : _) -> do
          drawCard iid drawFire
          shuffleCardsIntoDeck Deck.EncounterDeck [shuffleFire]
        [drawFire] -> do
          drawCard iid drawFire
        _ -> pure ()
      pure l
    _ -> Coop <$> liftRunMessage msg attrs
