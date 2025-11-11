module Arkham.Story.Cards.PackageDelivery (packageDelivery) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PackageDelivery = PackageDelivery StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

packageDelivery :: StoryCard PackageDelivery
packageDelivery = story PackageDelivery Cards.packageDelivery

instance RunMessage PackageDelivery where
  runMessage msg s@(PackageDelivery attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> PackageDelivery <$> liftRunMessage msg attrs
