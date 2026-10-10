module Arkham.Asset.Assets.SpringfieldM19034Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.PlayerCard qualified as PlayerCard
import Arkham.Event.Cards qualified as Events
import Arkham.Location.Base (
  connectedMatchersL,
  revealedConnectedMatchersL,
  revealedSymbolL,
  symbolL,
 )
import Arkham.Location.CardDefs.NightOfTheZealot.TheMidnightMasks qualified as Locations
import Arkham.LocationSymbol
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Taboo.Types
import TestImport.New

-- A chain of three: here -- middle -- yonder.
chainOfThree :: TestAppT (Location, Location, Location)
chainOfThree = do
  here <-
    testLocationWithDef Locations.rivertown
      $ (symbolL .~ Square)
      . (revealedSymbolL .~ Square)
      . (connectedMatchersL .~ [LocationWithSymbol Triangle])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
  middle <-
    testLocationWithDef Locations.southsideHistoricalSociety
      $ (symbolL .~ Triangle)
      . (revealedSymbolL .~ Triangle)
      . (connectedMatchersL .~ [LocationWithSymbol Square, LocationWithSymbol Moon])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Square, LocationWithSymbol Moon])
  yonder <-
    testLocationWithDef Locations.downtownFirstBankOfArkham
      $ (symbolL .~ Moon)
      . (revealedSymbolL .~ Moon)
      . (connectedMatchersL .~ [LocationWithSymbol Triangle])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
  for_ [here, middle, yonder] $ run . RevealLocation Nothing . toId
  pure (here, middle, yonder)

tabooedSpringfield :: Investigator -> TestAppT AssetId
tabooedSpringfield self = do
  card <-
    genPlayerCardWith Assets.springfieldM19034
      $ PlayerCard.setTaboo (Just TabooList19)
      . setPlayerCardOwner self.id
  run $ PutCardIntoPlay self.id (toCard card) Nothing NoPayment []
  selectJust $ assetIs Assets.springfieldM19034

spec :: Spec
spec = describe "Springfield M1903 (4)" do
  faq
    "The taboo reaches one location past the attack's standard range, so it stacks with anything that extends that range"
    do
      it "alone, cannot target an enemy two locations away" . gameTest $ \self -> do
        withProp @"combat" 5 self
        (here, _middle, yonder) <- chainOfThree
        self `moveTo` here
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
        enemy `spawnAt` yonder

        springfield <- tabooedSpringfield self
        (self `getActionsFrom` springfield) `shouldReturn` []

      it "with Telescopic Sight (3), targets a non-Elite enemy two locations away" . gameTest $ \self -> do
        withProp @"combat" 5 self
        withProp @"resources" 5 self
        setChaosTokens [Zero]

        (here, _middle, yonder) <- chainOfThree
        self `moveTo` here
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
        enemy `spawnAt` yonder

        springfield <- tabooedSpringfield self
        scopeCard <- genMyCard self Events.telescopicSight3
        self `playCard` scopeCard
        chooseTarget springfield -- attach to the two-handed Firearm
        scope <- selectJust $ eventIs Events.telescopicSight3

        [doFight] <- self `getActionsFrom` springfield
        self `useAbility` doFight
        useReactionOf scope -- exhaust Telescopic Sight to extend the range
        chooseTarget enemy
        startSkillTest
        applyResults
        enemy.damage `shouldReturn` 3

      it "with Marksmanship (1), targets a non-Elite enemy two locations away" . gameTest $ \self -> do
        withProp @"combat" 5 self
        withProp @"resources" 5 self
        setChaosTokens [Zero]

        (here, _middle, yonder) <- chainOfThree
        self `moveTo` here
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
        enemy `spawnAt` yonder

        -- the event goes to hand before the weapon enters play, so the next
        -- message's preloadEntities pass picks it up as an in-hand entity
        marksmanship <- genMyCard self Events.marksmanship1
        withProp @"hand" [marksmanship] self
        springfield <- tabooedSpringfield self

        [doFight] <- self `getActionsFrom` springfield
        self `useAbility` doFight
        chooseTarget marksmanship -- play Marksmanship from the fight window
        chooseTarget enemy
        startSkillTest
        applyResults
        -- 1 base + 2 from Springfield + 1 from Marksmanship (unengaged enemy)
        enemy.damage `shouldReturn` 4
