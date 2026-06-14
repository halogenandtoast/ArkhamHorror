module Arkham.Location.Cards.OtherworldlyMechanismsGrimeCoveredGears (otherworldlyMechanismsGrimeCoveredGears) where

import Arkham.Ability
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Glyph))

newtype OtherworldlyMechanismsGrimeCoveredGears = OtherworldlyMechanismsGrimeCoveredGears LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsGrimeCoveredGears :: LocationCard OtherworldlyMechanismsGrimeCoveredGears
otherworldlyMechanismsGrimeCoveredGears = location OtherworldlyMechanismsGrimeCoveredGears Cards.otherworldlyMechanismsGrimeCoveredGears 3 (Static 1)

instance HasAbilities OtherworldlyMechanismsGrimeCoveredGears where
  getAbilities (OtherworldlyMechanismsGrimeCoveredGears a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when You (be a)
      , restricted a 2 Here $ actionAbilityWithCost (ResourceCost 5)
      ]

instance RunMessage OtherworldlyMechanismsGrimeCoveredGears where
  runMessage msg l@(OtherworldlyMechanismsGrimeCoveredGears attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      glyphs <- getSetAsideCardsMatching (CardWithTrait Glyph <> #treachery)
      for_ (nonEmpty glyphs) $ sample >=> drawCard iid
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      activateLocation (attrs.ability 2) attrs.id
      pure l
    _ -> OtherworldlyMechanismsGrimeCoveredGears <$> liftRunMessage msg attrs
