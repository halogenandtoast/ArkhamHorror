module Arkham.Location.Cards.LodgeCellarMembersOnly (lodgeCellarMembersOnly) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LodgeCellarMembersOnly = LodgeCellarMembersOnly LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarMembersOnly :: LocationCard LodgeCellarMembersOnly
lodgeCellarMembersOnly = location LodgeCellarMembersOnly Cards.lodgeCellarMembersOnly 3 (Static 0)

instance HasModifiersFor LodgeCellarMembersOnly where
  getModifiersFor (LodgeCellarMembersOnly a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities LodgeCellarMembersOnly where
  getAbilities (LodgeCellarMembersOnly a) =
    extendUnrevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "lodgeCellarMembersOnly.action"
      $ restricted (proxied (LocationMatcherSource "Lodge Gates") a) 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (LocationWithTitle "Lodge Gates"))

instance RunMessage LodgeCellarMembersOnly where
  runMessage msg l@(LodgeCellarMembersOnly attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      revealBy iid attrs
      pure l
    _ -> LodgeCellarMembersOnly <$> liftRunMessage msg attrs
