module Arkham.Homebrew.AgainstTheWendigo.Acts.InSearchOfTheMissing (inSearchOfTheMissing) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype InSearchOfTheMissing = InSearchOfTheMissing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inSearchOfTheMissing :: ActCard InSearchOfTheMissing
inSearchOfTheMissing =
  act (1, A) InSearchOfTheMissing Cards.inSearchOfTheMissing (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage InSearchOfTheMissing where
  runMessage msg a@(InSearchOfTheMissing attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> InSearchOfTheMissing <$> liftRunMessage msg attrs
