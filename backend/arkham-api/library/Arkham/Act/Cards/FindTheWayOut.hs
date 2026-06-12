module Arkham.Act.Cards.FindTheWayOut (findTheWayOut) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Matcher

newtype FindTheWayOut = FindTheWayOut ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findTheWayOut :: ActCard FindTheWayOut
findTheWayOut = act (2, A) FindTheWayOut Cards.findTheWayOut Nothing

instance HasAbilities FindTheWayOut where
  getAbilities (FindTheWayOut a) =
    extend
      a
      [ restricted a 1 (OnLocation LocationWithoutClues) exploreAction_
      , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

instance RunMessage FindTheWayOut where
  runMessage msg a@(FindTheWayOut attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FindTheWayOut <$> liftRunMessage msg attrs
