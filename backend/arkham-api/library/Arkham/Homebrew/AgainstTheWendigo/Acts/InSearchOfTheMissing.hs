module Arkham.Homebrew.AgainstTheWendigo.Acts.InSearchOfTheMissing (inSearchOfTheMissing) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (drawStudentsFate)
import Arkham.Matcher

newtype InSearchOfTheMissing = InSearchOfTheMissing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inSearchOfTheMissing :: ActCard InSearchOfTheMissing
inSearchOfTheMissing =
  act (1, A) InSearchOfTheMissing Cards.inSearchOfTheMissing (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage InSearchOfTheMissing where
  runMessage msg a@(InSearchOfTheMissing attrs) = runQueueT $ case msg of
    -- "The lead investigator randomly takes a card from the Student's Fate deck
    -- and reads the first part of it."
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      drawStudentsFate
      advanceActDeck attrs
      pure a
    _ -> InSearchOfTheMissing <$> liftRunMessage msg attrs
