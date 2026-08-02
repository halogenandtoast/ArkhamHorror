module Arkham.Location.Cards.ShatteredRuins (shatteredRuins) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheWesternWall.Helpers (cannotEnterFromCluedLocation)

newtype ShatteredRuins = ShatteredRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredRuins :: LocationCard ShatteredRuins
shatteredRuins = withXShroud $ location ShatteredRuins Cards.shatteredRuins 0 (Static 2)

instance HasModifiersFor ShatteredRuins where
  getModifiersFor (ShatteredRuins a) = cannotEnterFromCluedLocation a

instance HasAbilities ShatteredRuins where
  getAbilities (ShatteredRuins a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a (not_ FloodedLocation)) actionAbility

instance RunMessage ShatteredRuins where
  runMessage msg l@(ShatteredRuins attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      campaignSpecific "translateGlyph" ("rune_v" :: Text, "Stranger" :: Text)
      pure l
    _ -> ShatteredRuins <$> liftRunMessage msg attrs
