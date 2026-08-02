module Arkham.Act.Cards.DeadlySkies (deadlySkies) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype DeadlySkies = DeadlySkies ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlySkies :: ActCard DeadlySkies
deadlySkies = act (2, A) DeadlySkies Cards.deadlySkies Nothing

instance HasAbilities DeadlySkies where
  getAbilities (DeadlySkies a) =
    extend
      a
      [ restricted a 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
      , restricted
          a
          2
          (EachUndefeatedInvestigator $ at_ $ locationIs Locations.floatingSpire)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage DeadlySkies where
  runMessage msg a@(DeadlySkies attrs) = runQueueT $ case msg of
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
      -- Defying Gravity: the spire falls away and the skyline reforms around
      -- Floating Spire, one row taller than before.
      selectEach (locationIs Locations.centralSpire) removeIgnoringTextBox
      floatingSpire <- selectJust $ locationIs Locations.floatingSpire
      rebuildSkyline floatingSpire actThreeLayout
      shuffleIntoSummitTop 3 =<< getSetAsideCardsMatching (cardIs Locations.westernWall_11651)
      advanceActDeck attrs
      pure a
    _ -> DeadlySkies <$> liftRunMessage msg attrs
