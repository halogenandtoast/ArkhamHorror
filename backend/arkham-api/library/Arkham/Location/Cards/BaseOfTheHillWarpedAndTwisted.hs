module Arkham.Location.Cards.BaseOfTheHillWarpedAndTwisted (baseOfTheHillWarpedAndTwisted) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Window
import Arkham.Scenarios.WhereDoomAwaits.Helpers

newtype BaseOfTheHillWarpedAndTwisted = BaseOfTheHillWarpedAndTwisted LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHillWarpedAndTwisted :: LocationCard BaseOfTheHillWarpedAndTwisted
baseOfTheHillWarpedAndTwisted =
  locationWith BaseOfTheHillWarpedAndTwisted Cards.baseOfTheHillWarpedAndTwisted 4 (Static 3)
    $ revealedConnectedMatchersL
    <>~ [LocationWithTitle "Diverging Path"]

instance HasModifiersFor BaseOfTheHillWarpedAndTwisted where
  getModifiersFor (BaseOfTheHillWarpedAndTwisted a) = do
    modifySelectWhen a a.revealed (LocationWithTitle "Diverging Path") [ConnectedToWhen (be a) Anywhere]

instance HasAbilities BaseOfTheHillWarpedAndTwisted where
  getAbilities (BaseOfTheHillWarpedAndTwisted a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "baseOfTheHill.resign" $ locationResignAction a
      , mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      ]

instance RunMessage BaseOfTheHillWarpedAndTwisted where
  runMessage msg l@(BaseOfTheHillWarpedAndTwisted attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      spendClues iid n
      divergingPaths <- getSetAsideCardsMatching $ CardWithTitle "Diverging Path"
      for_ (nonEmpty divergingPaths) (sampleN n >=> traverse_ placeLocation_)
      pure l
    _ -> BaseOfTheHillWarpedAndTwisted <$> liftRunMessage msg attrs
