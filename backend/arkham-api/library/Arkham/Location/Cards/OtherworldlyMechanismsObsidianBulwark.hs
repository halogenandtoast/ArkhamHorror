module Arkham.Location.Cards.OtherworldlyMechanismsObsidianBulwark (otherworldlyMechanismsObsidianBulwark) where

import Arkham.Ability
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Glyph))

newtype OtherworldlyMechanismsObsidianBulwark = OtherworldlyMechanismsObsidianBulwark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsObsidianBulwark :: LocationCard OtherworldlyMechanismsObsidianBulwark
otherworldlyMechanismsObsidianBulwark = location OtherworldlyMechanismsObsidianBulwark Cards.otherworldlyMechanismsObsidianBulwark 2 (Static 2)

instance HasAbilities OtherworldlyMechanismsObsidianBulwark where
  getAbilities (OtherworldlyMechanismsObsidianBulwark a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when You (be a)
      , restricted a 2 Here $ actionAbilityWithCost (HandDiscardCost 3 #any)
      ]

instance RunMessage OtherworldlyMechanismsObsidianBulwark where
  runMessage msg l@(OtherworldlyMechanismsObsidianBulwark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      glyphs <- getSetAsideCardsMatching (CardWithTrait Glyph <> #treachery)
      for_ (nonEmpty glyphs) $ sample >=> drawCard iid
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      activateLocation (attrs.ability 2) attrs.id
      pure l
    _ -> OtherworldlyMechanismsObsidianBulwark <$> liftRunMessage msg attrs
