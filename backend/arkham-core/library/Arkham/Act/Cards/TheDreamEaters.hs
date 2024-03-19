module Arkham.Act.Cards.TheDreamEaters
  ( TheDreamEaters(..)
  , theDreamEaters
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheDreamEaters = TheDreamEaters ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theDreamEaters :: ActCard TheDreamEaters
theDreamEaters = act (5, A) TheDreamEaters Cards.theDreamEaters Nothing

instance RunMessage TheDreamEaters where
  runMessage msg a@(TheDreamEaters attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheDreamEaters <$> lift (runMessage msg attrs)
