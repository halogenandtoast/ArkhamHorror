module Arkham.Act.Cards.UnsettlingSigns (unsettlingSigns) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Query (getInvestigators, getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.TheApiary.Helpers
import Arkham.Trait (Trait (Sanctum))

newtype UnsettlingSigns = UnsettlingSigns ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettlingSigns :: ActCard UnsettlingSigns
unsettlingSigns = act (1, A) UnsettlingSigns Cards.unsettlingSigns Nothing

instance HasAbilities UnsettlingSigns where
  getAbilities (UnsettlingSigns x) =
    extend
      x
      [ restricted x 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
      , restricted
          x
          2
          (EachUndefeatedInvestigator $ at_ $ withTrait Sanctum)
          $ Objective
          $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 2) Anywhere)
      ]

instance RunMessage UnsettlingSigns where
  runMessage msg a@(UnsettlingSigns attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      total <- getSpendableClueCount investigators
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 1 total attrs
      pure a
    ResolveAmounts _iid (getChoiceAmount "$clues" -> cluesSpent) (isTarget attrs -> True) | cluesSpent > 0 -> do
      investigators <- getInvestigators
      spendCluesAsAGroup investigators cluesSpent
      n <- getPlayerCount
      let perClue = if n == 1 then 10 else 5
      lead <- getLead
      discardTopOfEncounterDeckAndHandle lead attrs (cluesSpent * perClue) attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let locationCards = filterLocations cards
      focusCards locationCards $ for_ locationCards (drawCard iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      eachInvestigator (`loseAllClues` attrs)
      atLostCampsite <- selectAny $ InvestigatorAt (locationIs Locations.lostCampsite)
      if atLostCampsite
        then advanceToAct attrs Acts.lostPilgrims A
        else advanceToAct attrs Acts.theHiveMind A
      pure a
    _ -> UnsettlingSigns <$> liftRunMessage msg attrs
