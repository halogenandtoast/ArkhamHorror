module Arkham.Act.Cards.SearchingTheSpires (searchingTheSpires) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreation (..))
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype SearchingTheSpires = SearchingTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingTheSpires :: ActCard SearchingTheSpires
searchingTheSpires = act (1, A) SearchingTheSpires Cards.searchingTheSpires Nothing

instance HasAbilities SearchingTheSpires where
  getAbilities (SearchingTheSpires x) =
    extend
      x
      [ restricted x 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
      , restricted
          x
          2
          (EachUndefeatedInvestigator $ at_ (locationIs Locations.centralSpire))
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage SearchingTheSpires where
  runMessage msg a@(SearchingTheSpires attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- [action] Spend X clues. The X is chosen here; the actual reveal of X
      -- cards from the bottom of the Summit deck (and the optional place-an-open-
      -- sky-location-and-move-to-it follow-up) has no engine support yet.
      investigators <- getInvestigators
      total <- getSpendableClueCount investigators
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 1 total attrs
      pure a
    ResolveAmounts _iid (getChoiceAmount "$clues" -> cluesSpent) (isTarget attrs -> True) | cluesSpent > 0 -> do
      investigators <- getInvestigators
      spendCluesAsAGroup investigators cluesSpent
      -- TODO: Summit deck has no engine support. Once it exists, reveal `cluesSpent`
      -- cards from the BOTTOM of the Summit deck; the active investigator may put 1
      -- revealed location into play in an adjacent open sky and move to it, then place
      -- the open-sky card + the other revealed cards on top of the Summit deck.
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Spawn the set-aside The Inescapable at the Central Spire, exhausted and
      -- unengaged (created at a location with no investigator engagement).
      createSetAsideEnemyWith_
        Enemies.theInescapable
        (locationIs Locations.centralSpire)
        \c -> c {enemyCreationExhausted = True}
      -- Shuffle the rest of the The Inescapable encounter set (everything except the
      -- enemy we just spawned) plus the encounter discard back into the encounter deck.
      rest <-
        getSetAsideCardsMatching
          $ CardFromEncounterSet Set.TheInescapable
          <> not_ (cardIs Enemies.theInescapable)
      shuffleCardsIntoDeck Deck.EncounterDeck rest
      shuffleEncounterDiscardBackIn
      -- TODO: remove the R'lyeh-Streets locations, rebuild the Summit deck, and handle
      -- open-sky placement on advance. None of the Summit-deck / open-sky / sliding-
      -- location infrastructure has engine support yet.
      advanceActDeck attrs
      pure a
    _ -> SearchingTheSpires <$> liftRunMessage msg attrs
