module Arkham.Act.Cards.SearchingTheSpires (searchingTheSpires) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreation (..))
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
      investigators <- getInvestigators
      total <- getSpendableClueCount investigators
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 1 total attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> cluesSpent) (isTarget attrs -> True) | cluesSpent > 0 -> do
      investigators <- getInvestigators
      spendCluesAsAGroup investigators cluesSpent
      searchTheSpires (attrs.ability 1) iid cluesSpent
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

      -- The Sprawl: the streets fall away and the skyline reforms around Central
      -- Spire.
      selectEach (locationIs Locations.rlyehStreets) removeIgnoringTextBox
      centralSpire <- selectJust $ locationIs Locations.centralSpire
      rebuildSkyline centralSpire actTwoLayout
      -- "along with each remaining set-aside open sky card"
      scenarioSpecific "shuffleSearchingAct2Summit" ()
      advanceActDeck attrs
      pure a
    _ -> SearchingTheSpires <$> liftRunMessage msg attrs
