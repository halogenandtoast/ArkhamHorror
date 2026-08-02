module Arkham.Act.Cards.ReturnToTheShoreline (returnToTheShoreline) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype ReturnToTheShoreline = ReturnToTheShoreline ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTheShoreline :: ActCard ReturnToTheShoreline
returnToTheShoreline = act (3, A) ReturnToTheShoreline Cards.returnToTheShoreline Nothing

instance HasAbilities ReturnToTheShoreline where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (ScenarioDeckWithCard SummitDeck) $ actionAbilityWithCost ClueCostX
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage ReturnToTheShoreline where
  runMessage msg a@(ReturnToTheShoreline attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> n) -> do
      searchTheSpires (attrs.ability 1) iid n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ReturnToTheShoreline <$> liftRunMessage msg attrs
