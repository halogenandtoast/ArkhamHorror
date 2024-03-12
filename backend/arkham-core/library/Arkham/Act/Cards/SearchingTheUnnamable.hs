module Arkham.Act.Cards.SearchingTheUnnamable (SearchingTheUnnamable (..), searchingTheUnnamable) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner hiding (advanceActDeck, placeLabeledLocations)
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Placement
import Arkham.Prelude

newtype SearchingTheUnnamable = SearchingTheUnnamable ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchingTheUnnamable :: ActCard SearchingTheUnnamable
searchingTheUnnamable = act (1, A) SearchingTheUnnamable Cards.searchingTheUnnamable Nothing

instance RunMessage SearchingTheUnnamable where
  runMessage msg a@(SearchingTheUnnamable attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> runQueueT do
      -- Set The Unnamable aside. It keeps all tokens and attachments.
      mTheUnnamable <- selectOne $ enemyIs Enemies.theUnnamable
      for_ mTheUnnamable $ \theUnnamable -> do
        push $ PlaceEnemy theUnnamable Unplaced

      -- Remove each location from the game (or place it in the victory display
      -- if it has Victory X and no clues on it). Investigators are not
      -- defeated during this process. (Other enemies and attachments at those
      -- locations are discarded.)
      investigators <- allInvestigators
      allLocations <- select Anywhere
      victoryLocations <- select (RevealedLocation <> LocationWithoutClues <> LocationWithVictory)
      pushAll [PlaceInvestigator iid Unplaced | iid <- investigators]
      pushAll $ map (AddToVictory . toTarget) victoryLocations
      pushAll [RemoveLocation location | location <- allLocations, location `notElem` victoryLocations]

      -- Shuffle the set-aside Mysterious Stairs locations and put 5 of them
      -- into play in a vertical line. Remove the remaining Mysterious Stairs
      -- location from the game.
      mysteriousStairCards <- fmap (take 5) . shuffleM =<< getSetAsideCardsMatching "Mysterious Stairs"
      mysteriousStairs <- placeLabeledLocations "mysteriousStairs" mysteriousStairCards
      pushAll
        [ PlacedLocationDirection l1 Above l2
        | (l1, l2) <- zip mysteriousStairs (drop 1 mysteriousStairs)
        ]

      -- Place each investigator and the set-aside The Unnamable enemy at the
      -- topmost Mysterious Stairs location, revealing it.
      for_ (take 1 mysteriousStairs) $ \topmostStairs -> do
        pushAll [MoveAllTo (toSource attrs) topmostStairs]
        for_ mTheUnnamable $ \theUnnamable -> do
          push $ PlaceEnemy theUnnamable (AtLocation topmostStairs)

      -- Search the encounter deck and discard pile for both copies of Locked
      -- Door and both copies of Secrets in the Attic, and remove them from the
      -- game.
      pushAll
        [ RemoveAllCopiesOfEncounterCardFromGame "Locked Door"
        , RemoveAllCopiesOfEncounterCardFromGame "Secrets in the Attic"
        ]

      -- Shuffle each set-aside copy of Endless Descent into the encounter
      -- deck, along with the encounter discard pile.
      endlessDescent <- getSetAsideCardsMatching "Endless Descent"
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck endlessDescent, ShuffleEncounterDiscardBackIn]

      advanceActDeck attrs
      pure a
    _ -> SearchingTheUnnamable <$> runMessage msg attrs
