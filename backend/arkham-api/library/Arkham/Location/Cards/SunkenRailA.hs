module Arkham.Location.Cards.SunkenRailA (sunkenRailA) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SunkenRailA = SunkenRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenRailA :: LocationCard SunkenRailA
sunkenRailA = location SunkenRailA Cards.sunkenRailA 4 (PerPlayer 2)

instance HasAbilities SunkenRailA where
  getAbilities (SunkenRailA a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage SunkenRailA where
  runMessage msg l@(SunkenRailA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      for_ mcard (drawCard iid)
      pure l
    _ -> SunkenRailA <$> liftRunMessage msg attrs
