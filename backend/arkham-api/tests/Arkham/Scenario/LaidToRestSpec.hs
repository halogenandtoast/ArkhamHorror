module Arkham.Scenario.LaidToRestSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (assetIs)
import Arkham.Question
import Data.Aeson.Types (parseMaybe)
import TestImport

-- | Laid to Rest (side-story 90054) requires Jim Culver to have a spirit deck
-- and The Beyond, Bleak Netherworld in play. Parallel Jim brings both himself;
-- a base Jim (02004) does not, so setup must prompt the player to build a
-- spirit deck (9 different Ally assets, level 0-2, any class) and then put The
-- Beyond into play, which shuffles the side deck into its spirit deck. The four
-- Heretics added at setup must also end up in that spirit deck.
spec :: Spec
spec = describe "Laid to Rest (spirit deck construction)" do
  it "prompts a non-parallel Jim to build a spirit deck and loads The Beyond"
    . scenarioTestWith Investigators.jimCulver "90054"
    $ \jim -> do
      -- Building the spirit deck is part of Jim's investigator setup.
      pushAndRun $ SetupInvestigator (toId jim)

      -- The build question should be pending for Jim's player (keyed by PlayerId).
      pid <- getPlayer (toId jim)
      mQuestion <- lookup pid . gameQuestion <$> getGame
      codes <- case mQuestion >>= unwrapPickScenarioSpecific of
        Just ("laidToRest.buildSpiritDeck", v) -> pure (parseCardCodes v)
        _ -> do
          liftIO $ expectationFailure "expected laidToRest.buildSpiritDeck question"
          pure []

      liftIO $ length codes `shouldSatisfy` (>= 9)

      -- Answer with the first 9 candidate codes (simulates ScenarioSpecificAnswer).
      pushAndRun
        $ ScenarioSpecific "laidToRest.buildSpiritDeck"
        $ object ["cardCodes" .= take 9 codes]

      -- The Beyond is now in play.
      assert $ selectAny (assetIs Assets.theBeyondBleakNetherworld)

      -- Run scenario setup, which shuffles the 4 Heretics into the spirit deck.
      pushAndRun Setup

      -- 9 chosen allies + Vengeful Shade (auto-added) + 4 Heretics.
      spiritDeckSize <- theBeyondSpiritDeckSize
      liftIO $ spiritDeckSize `shouldBe` 14
 where
  unwrapPickScenarioSpecific = \case
    QuestionLabel _ _ q -> unwrapPickScenarioSpecific q
    PayCostQuestion _ q -> unwrapPickScenarioSpecific q
    QuestionWithSource _ _ q -> unwrapPickScenarioSpecific q
    PickScenarioSpecific k v -> Just (k, v)
    _ -> Nothing

  parseCardCodes :: Value -> [CardCode]
  parseCardCodes v = fromMaybe [] $ parseMaybe (withObject "candidates" (.: "cardCodes")) v

  theBeyondSpiritDeckSize :: TestAppT Int
  theBeyondSpiritDeckSize = do
    assets <- toList . entitiesAssets . gameEntities <$> getGame
    let spiritDeckOf asset =
          parseMaybe (withObject "asset" (.: "spiritDeck")) (toJSON asset) :: Maybe [Value]
    pure $ maybe 0 length $ asum (map spiritDeckOf assets)
