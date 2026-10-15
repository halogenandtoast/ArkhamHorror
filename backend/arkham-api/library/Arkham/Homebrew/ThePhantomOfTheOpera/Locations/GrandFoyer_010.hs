module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.GrandFoyer_010 (grandFoyer_010) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandFoyer_010 = GrandFoyer_010 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandFoyer_010 :: LocationCard GrandFoyer_010
grandFoyer_010 = symbolLabel $ location GrandFoyer_010 Cards.grandFoyer_010 3 (Static 0)

instance HasAbilities GrandFoyer_010 where
  getAbilities (GrandFoyer_010 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "grandFoyer.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandFoyer_010.parley"
          $ groupLimit PerRound
          $ restricted
            a
            1
            (Here <> exists (ConcealedCardAt (LocationWithTitle "Grand Foyer")))
            parleyAction_
      ]

instance RunMessage GrandFoyer_010 where
  runMessage msg l@(GrandFoyer_010 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      foyers <- select $ LocationWithTitle "Grand Foyer"
      gathered <- concatForM foyers (getConcealedAtAll NotForExpose)
      let (decoys, others) = partition (\c -> c.isDecoy) gathered
      for_ (take 1 decoys) $ removeFromGame . toTarget
      remaining <- shuffle (drop 1 decoys <> others)
      distributeEvenlyBetween remaining foyers
      pure l
    _ -> GrandFoyer_010 <$> liftRunMessage msg attrs
