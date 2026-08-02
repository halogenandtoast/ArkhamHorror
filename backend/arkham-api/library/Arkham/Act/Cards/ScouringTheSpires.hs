module Arkham.Act.Cards.ScouringTheSpires (scouringTheSpires) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype ScouringTheSpires = ScouringTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scouringTheSpires :: ActCard ScouringTheSpires
scouringTheSpires = act (1, A) ScouringTheSpires Cards.scouringTheSpires Nothing

instance HasAbilities ScouringTheSpires where
  getAbilities (ScouringTheSpires attrs) =
    extend
      attrs
      [ restricted attrs 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
      , restricted
          attrs
          2
          (notExists $ UneliminatedInvestigator <> not_ (InvestigatorAt $ locationIs Locations.centralSpire))
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage ScouringTheSpires where
  runMessage msg a@(ScouringTheSpires attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      total <- getSpendableClueCount investigators
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 1 total attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> x) (isTarget attrs -> True) | x > 0 -> do
      investigators <- getInvestigators
      spendCluesAsAGroup investigators x
      searchTheSpires (attrs.ability 1) iid x
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Floating Skyline: the streets fall away and the skyline reforms around
      -- Central Spire.
      selectEach (locationIs Locations.rlyehStreets) removeIgnoringTextBox
      centralSpire <- selectJust $ locationIs Locations.centralSpire
      rebuildSkyline centralSpire actTwoLayout
      shuffleIntoSummitTop 3
        =<< getSetAsideCardsMatching
          (mapOneOf cardIs [Locations.floatingSpire, Locations.aerialWaterfall])
      advanceActDeck attrs
      pure a
    _ -> ScouringTheSpires <$> liftRunMessage msg attrs
