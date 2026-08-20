module Arkham.Location.Cards.TheDrownedCity.TheWesternWall.SunkenStairway (sunkenStairway) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDrownedCity.TheWesternWall.Helpers (cannotEnterFromCluedLocation, scenarioI18n)

newtype SunkenStairway = SunkenStairway LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenStairway :: LocationCard SunkenStairway
sunkenStairway = withXShroud $ location SunkenStairway Cards.sunkenStairway 0 (Static 2)

instance HasModifiersFor SunkenStairway where
  getModifiersFor (SunkenStairway a) = cannotEnterFromCluedLocation a

instance HasAbilities SunkenStairway where
  getAbilities (SunkenStairway a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage SunkenStairway where
  runMessage msg l@(SunkenStairway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ (locationPosition attrs) \(Pos _ row) -> do
        card <- getSetAsideCard Cards.underseaVault
        positions <- catMaybes <$> selectField LocationPosition Anywhere
        let
          availablePosition targetRow =
            let usedColumns = [pos.column | pos <- positions, pos.row == targetRow]
             in Pos
                  (fromJustNote "No available grid position" $ find (`notElem` usedColumns) [0 ..])
                  targetRow
        scenarioI18n $ scope "sunkenStairway" $ chooseOneM iid do
          labeled' "above" $ placeLocationInGrid_ (availablePosition $ row + 1) card
          labeled' "below" $ placeLocationInGrid_ (availablePosition $ row - 1) card
      pure l
    _ -> SunkenStairway <$> liftRunMessage msg attrs
