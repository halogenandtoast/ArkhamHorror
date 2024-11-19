module Arkham.Act.Cards.SearchForACampSite (SearchForACampSite (..), searchForACampSite) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype SearchForACampSite = SearchForACampSite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForACampSite :: ActCard SearchForACampSite
searchForACampSite = act (1, A) SearchForACampSite Cards.searchForACampSite Nothing

instance HasAbilities SearchForACampSite where
  getAbilities (SearchForACampSite a) =
    [ restricted
        a
        1
        ( EachUndefeatedInvestigator
            $ InvestigatorAt
            $ LocationWithInvestigator LeadInvestigator
            <> LocationWithoutClues
        )
        $ ActionAbility [#resign] (ActionCost 1)
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage SearchForACampSite where
  runMessage msg a@(SearchForACampSite attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachInvestigator resign
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> SearchForACampSite <$> liftRunMessage msg attrs
