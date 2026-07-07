module Arkham.Location.Cards.ShatteredRuins (shatteredRuins) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype ShatteredRuins = ShatteredRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredRuins :: LocationCard ShatteredRuins
shatteredRuins = location ShatteredRuins Cards.shatteredRuins 0 (Static 2)

instance HasAbilities ShatteredRuins where
  getAbilities (ShatteredRuins a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a (not_ FloodedLocation)) actionAbility

instance RunMessage ShatteredRuins where
  runMessage msg l@(ShatteredRuins attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      campaignSpecific "translateGlyph" ("rune_v" :: Text, "Stranger" :: Text)
      record TheInvestigatorsDiscoveredAnAlienLanguage
      pure l
    _ -> ShatteredRuins <$> liftRunMessage msg attrs
