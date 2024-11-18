module Arkham.Act.Cards.SearchForACampSite (SearchForACampSite (..), searchForACampSite) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchForACampSite = SearchForACampSite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForACampSite :: ActCard SearchForACampSite
searchForACampSite = act (1, A) SearchForACampSite Cards.searchForACampSite Nothing

instance RunMessage SearchForACampSite where
  runMessage msg a@(SearchForACampSite attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchForACampSite <$> liftRunMessage msg attrs
