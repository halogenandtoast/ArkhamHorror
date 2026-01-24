module Arkham.Location.Cards.SunkenRailB (sunkenRailB) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SunkenRailB = SunkenRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenRailB :: LocationCard SunkenRailB
sunkenRailB = symbolLabel $ location SunkenRailB Cards.sunkenRailB 4 (PerPlayer 2)

instance HasAbilities SunkenRailB where
  getAbilities (SunkenRailB a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage SunkenRailB where
  runMessage msg l@(SunkenRailB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      for_ mcard (drawCard iid)
      pure l
    _ -> SunkenRailB <$> liftRunMessage msg attrs
