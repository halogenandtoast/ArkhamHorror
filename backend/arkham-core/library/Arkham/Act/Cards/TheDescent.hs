module Arkham.Act.Cards.TheDescent
  ( TheDescent(..)
  , theDescent
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheDescent = TheDescent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theDescent :: ActCard TheDescent
theDescent = act (2, A) TheDescent Cards.theDescent Nothing

instance RunMessage TheDescent where
  runMessage msg a@(TheDescent attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      pure a
    _ -> TheDescent <$> lift (runMessage msg attrs)
