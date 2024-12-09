module Arkham.Location.Cards.Serannian (serannian, Serannian (..)) where

import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Story.Cards qualified as Story

newtype Serannian = Serannian LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serannian :: LocationCard Serannian
serannian = location Serannian Cards.serannian 3 (PerPlayer 1)

instance HasModifiersFor Serannian where
  getModifiersFor (Serannian a) = whenRevealed a do
    modifySelf
      a
      [ AdditionalCostToEnter $ HandDiscardCost 1 #any
      , AdditionalCostToLeave $ HandDiscardCost 1 #any
      ]

instance HasAbilities Serannian where
  getAbilities (Serannian attrs) = veiled attrs []

instance RunMessage Serannian where
  runMessage msg (Serannian attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.timelessBeauty
      pure . Serannian $ attrs & canBeFlippedL .~ False
    _ -> Serannian <$> liftRunMessage msg attrs
