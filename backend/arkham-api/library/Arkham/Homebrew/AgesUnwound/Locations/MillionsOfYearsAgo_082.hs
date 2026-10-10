module Arkham.Homebrew.AgesUnwound.Locations.MillionsOfYearsAgo_082 (millionsOfYearsAgo_082) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Dinosaur))

newtype MillionsOfYearsAgo_082 = MillionsOfYearsAgo_082 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Millions of Years Ago/, the printing that keeps the jungle stocked.
millionsOfYearsAgo_082 :: LocationCard MillionsOfYearsAgo_082
millionsOfYearsAgo_082 =
  locationWith MillionsOfYearsAgo_082 Cards.millionsOfYearsAgo_082 2 (PerPlayer 2)
    $ connectsToL
    .~ ringConnections

-- | "Enemies at Millions of Years Ago get +1 fight and -1 evade."
instance HasModifiersFor MillionsOfYearsAgo_082 where
  getModifiersFor (MillionsOfYearsAgo_082 a) =
    modifySelect a (EnemyAt $ be a) [EnemyFight 1, EnemyEvade (-1)]

{- | "Forced - At the end of your turn, if there are no [[Dinosaur]] enemies at
this location: Search the encounter deck and discard pile for a [[Dinosaur]]
enemy and spawn it at this location, exhausted. Shuffle the encounter deck."
-}
instance HasAbilities MillionsOfYearsAgo_082 where
  getAbilities (MillionsOfYearsAgo_082 a) =
    extendRevealed1 a
      $ restricted
        a
        endOfTurnAbility
        (Here <> notExists (EnemyWithTrait Dinosaur <> enemyAt a.id))
      $ forced
      $ TurnEnds #when You

instance RunMessage MillionsOfYearsAgo_082 where
  runMessage msg l@(MillionsOfYearsAgo_082 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs (card_ $ #enemy <> CardWithTrait Dinosaur)
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAtEdit_ card attrs.id createExhausted
      shuffleDeck Deck.EncounterDeck
      pure l
    _ -> MillionsOfYearsAgo_082 <$> liftRunMessage msg attrs
