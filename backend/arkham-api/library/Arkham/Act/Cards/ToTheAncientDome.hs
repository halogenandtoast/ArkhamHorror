module Arkham.Act.Cards.ToTheAncientDome (toTheAncientDome) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype ToTheAncientDome = ToTheAncientDome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheAncientDome :: ActCard ToTheAncientDome
toTheAncientDome = act (2, A) ToTheAncientDome Cards.toTheAncientDome Nothing

instance HasAbilities ToTheAncientDome where
  getAbilities (ToTheAncientDome a) =
    extend
      a
      [ restricted a 1 (ScenarioDeckWithCard SummitDeck) $ actionAbilityWithCost ClueCostX
      , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

instance RunMessage ToTheAncientDome where
  runMessage msg a@(ToTheAncientDome attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> n) -> do
      searchTheSpires (attrs.ability 1) iid n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> ToTheAncientDome <$> liftRunMessage msg attrs
