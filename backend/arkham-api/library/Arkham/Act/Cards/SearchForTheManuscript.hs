module Arkham.Act.Cards.SearchForTheManuscript (searchForTheManuscript) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchForTheManuscript = SearchForTheManuscript ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheManuscript :: ActCard SearchForTheManuscript
searchForTheManuscript = act (1, A) SearchForTheManuscript Cards.searchForTheManuscript Nothing

instance RunMessage SearchForTheManuscript where
  runMessage msg a@(SearchForTheManuscript attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchForTheManuscript <$> liftRunMessage msg attrs
