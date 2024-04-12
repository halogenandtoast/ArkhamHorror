{-# LANGUAGE MultiWayIf #-}

module Arkham.Treachery.Cards.Greed (greed, Greed (..)) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Greed = Greed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greed :: TreacheryCard Greed
greed = treachery Greed Cards.greed

instance RunMessage Greed where
  runMessage msg t@(Greed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      resources <- field InvestigatorResources iid
      let
        x
          | resources == 0 = 4
          | resources <= 5 = 3
          | resources <= 10 = 2
          | otherwise = 1
      assignHorror iid attrs x
      pure t
    _ -> Greed <$> lift (runMessage msg attrs)
