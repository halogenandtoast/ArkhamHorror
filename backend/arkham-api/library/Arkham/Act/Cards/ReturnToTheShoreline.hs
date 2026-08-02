module Arkham.Act.Cards.ReturnToTheShoreline (returnToTheShoreline) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype ReturnToTheShoreline = ReturnToTheShoreline ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTheShoreline :: ActCard ReturnToTheShoreline
returnToTheShoreline = act (3, A) ReturnToTheShoreline Cards.returnToTheShoreline Nothing

instance HasAbilities ReturnToTheShoreline where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ InvestigatorWithClues $ atLeast 1) actionAbility
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage ReturnToTheShoreline where
  runMessage msg a@(ReturnToTheShoreline attrs) = runQueueT $ case msg of
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
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ReturnToTheShoreline <$> liftRunMessage msg attrs
