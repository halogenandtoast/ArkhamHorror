module Arkham.Location.Cards.InnerSanctum (innerSanctum, InnerSanctum (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype InnerSanctum = InnerSanctum LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innerSanctum :: LocationCard InnerSanctum
innerSanctum = location InnerSanctum Cards.innerSanctum 4 (PerPlayer 1)

instance HasModifiersFor InnerSanctum where
  getModifiersFor (InnerSanctum attrs) = whenUnrevealed attrs do
    modifySelect attrs (not_ $ InvestigatorWithKey CultistKey) [CannotEnter (toId attrs)]

instance HasAbilities InnerSanctum where
  getAbilities (InnerSanctum attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #after You (be attrs)

instance RunMessage InnerSanctum where
  runMessage msg l@(InnerSanctum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mKey <- getRandomKey
      for_ mKey (placeKey attrs)
      pure l
    _ -> InnerSanctum <$> liftRunMessage msg attrs
