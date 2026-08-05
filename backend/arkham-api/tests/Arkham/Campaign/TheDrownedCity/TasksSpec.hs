module Arkham.Campaign.TheDrownedCity.TasksSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.Helpers (tasks)
import Arkham.Card.CardDef
import Arkham.Name (toTitle)
import Arkham.Trait
import TestImport.New

{- | The completed back for each Task front, listed independently of the campaign's
own 'completedTask' mapping so this stays a check on the card data.
-}
completedSides :: [(CardDef, CardDef)]
completedSides =
  [ (Assets.noPlaceLikeHome, Assets.noPlaceLikeHomeCompleted)
  , (Assets.walkInFaith, Assets.walkInFaithCompleted)
  , (Assets.toeTheLine, Assets.toeTheLineCompleted)
  , (Assets.goodMoney, Assets.goodMoneyCompleted)
  , (Assets.proveYourWorth, Assets.proveYourWorthCompleted)
  , (Assets.doNoHarm, Assets.doNoHarmCompleted)
  , (Assets.dreamsOfDestruction, Assets.dreamsOfDestructionCompleted)
  , (Assets.plumbTheDepths, Assets.plumbTheDepthsCompleted)
  ]

title :: CardDef -> String
title = unpack . toTitle . cdName

spec :: Spec
spec = describe "The Drowned City Tasks" do
  it "registers all eight Tasks" (length tasks `shouldBe` 8 :: IO ())

  it "pairs every registered Task with a completed side" do
    ( sort [title def | (_, def, _) <- tasks] `shouldBe` sort (map (title . fst) completedSides)
        :: IO ()
      )

  -- Each Task is a double-sided permanent: the incomplete front is a weakness
  -- (subtype_code "weakness" in the card data), the completed back is not.
  for_ tasks \(_, def, label) -> describe (title def) do
    it "enters play on a weakness front" (cdCardSubType def `shouldBe` Just Weakness :: IO ())
    it "is a permanent Task, Incomplete card" do
      (cdPermanent def `shouldBe` True :: IO ())
      (cdCardTraits def `shouldBe` setFromList [Task, Incomplete] :: IO ())
    it "has an i18n label" (label `shouldSatisfy` (not . null) :: IO ())

  for_ completedSides \(front, completed) -> describe (title front <> " (completed)") do
    it "is not a weakness" (cdCardSubType completed `shouldBe` Nothing :: IO ())
    it "is a permanent Task, Completed card" do
      (cdPermanent completed `shouldBe` True :: IO ())
      (cdCardTraits completed `shouldBe` setFromList [Task, Completed] :: IO ())
