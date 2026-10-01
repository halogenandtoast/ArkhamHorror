module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_174 (
  shadowedWilderness_174,
) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ShadowedWilderness_174 = ShadowedWilderness_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedWilderness_174 :: LocationCard ShadowedWilderness_174
shadowedWilderness_174 = location ShadowedWilderness_174 Cards.shadowedWilderness_174 2 (PerPlayer 1)

-- "from Shadowed Wilderness" is self-referential text, so it is this copy only, not the
-- other four (rules/glossary/self-referential_text).
instance HasAbilities ShadowedWilderness_174 where
  getAbilities (ShadowedWilderness_174 a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage ShadowedWilderness_174 where
  runMessage msg l@(ShadowedWilderness_174 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (ShroudModifier 2)
      pure l
    _ -> ShadowedWilderness_174 <$> liftRunMessage msg attrs
