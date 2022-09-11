module Arkham.Act.Cards.StrangeRelicsMariasInformation
  ( StrangeRelicsMariasInformation(..)
  , strangeRelicsMariasInformation
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype StrangeRelicsMariasInformation = StrangeRelicsMariasInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

strangeRelicsMariasInformation :: ActCard StrangeRelicsMariasInformation
strangeRelicsMariasInformation = act
  (2, E)
  StrangeRelicsMariasInformation
  Cards.strangeRelicsMariasInformation
  Nothing

instance RunMessage StrangeRelicsMariasInformation where
  runMessage msg (StrangeRelicsMariasInformation attrs) =
    StrangeRelicsMariasInformation <$> runMessage msg attrs
