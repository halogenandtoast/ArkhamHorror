module Arkham.Location.Cards.SnowCoveredCrag (snowCoveredCrag) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype SnowCoveredCrag = SnowCoveredCrag LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowCoveredCrag :: LocationCard SnowCoveredCrag
snowCoveredCrag =
  locationWith
    SnowCoveredCrag
    Cards.snowCoveredCrag
    2
    (PerPlayer 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities SnowCoveredCrag where
  getAbilities (SnowCoveredCrag a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance HasModifiersFor SnowCoveredCrag where
  getModifiersFor (SnowCoveredCrag l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage SnowCoveredCrag where
  runMessage msg l@(SnowCoveredCrag attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid
      chooseOneAtATimeM iid $ targets assets exhaustThis
      pure l
    _ -> SnowCoveredCrag <$> liftRunMessage msg attrs
