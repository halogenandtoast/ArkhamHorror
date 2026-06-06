module Arkham.Event.Events.Transfiguration2Spec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Transfiguration (2)" do
  it "does not offer bonded investigator cards" . gameTest $ \self -> do
    self `playEvent` Events.transfiguration2
    offered <- offeredInvestigators
    offered `shouldSatisfy` notNull
    offered `shouldSatisfy` elem "10015"
    for_ ["10016a", "10016b", "11068b"] \bonded ->
      offered `shouldSatisfy` notElem bonded

  it "treats the front of your investigator card as the chosen card" . gameTest $ \self -> do
    self `playEvent` Events.transfiguration2
    chooseHankSamson
    self.willpower `shouldReturn` 3
    self.intellect `shouldReturn` 1
    self.combat `shouldReturn` 5
    self.agility `shouldReturn` 3

offeredInvestigators :: TestAppT [CardCode]
offeredInvestigators = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, ChooseOne msgs)] -> pure [code | CardLabel code _ _ <- msgs]
    q -> error $ "expected a single ChooseOne, got: " <> show q

chooseHankSamson :: TestAppT ()
chooseHankSamson = chooseOptionMatching "choose Hank Samson" \case
  CardLabel code _ _ -> code == "10015"
  _ -> False
