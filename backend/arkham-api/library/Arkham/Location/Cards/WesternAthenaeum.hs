module Arkham.Location.Cards.WesternAthenaeum (westernAthenaeum) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype WesternAthenaeum = WesternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernAthenaeum :: LocationCard WesternAthenaeum
westernAthenaeum = location WesternAthenaeum Cards.westernAthenaeum 5 (Static 1)

instance HasAbilities WesternAthenaeum where
  getAbilities (WesternAthenaeum a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ freeReaction
      $ DiscoveringLastClue #after You (be a)

instance RunMessage WesternAthenaeum where
  runMessage msg l@(WesternAthenaeum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "After you discover the last clue at this location: discover glyph
      -- rune_c, record Elder Thing." Glyph discovery is modeled the same way as
      -- the other Drowned City Glyph locations (mirror UnderseaVault): the
      -- campaign translateGlyph handler records the translation and we record
      -- that an alien language was discovered.
      campaignSpecific "translateGlyph" ("rune_c" :: Text, "Elder Thing" :: Text)
      record TheInvestigatorsDiscoveredAnAlienLanguage
      pure l
    _ -> WesternAthenaeum <$> liftRunMessage msg attrs
