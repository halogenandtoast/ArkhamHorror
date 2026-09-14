module Arkham.Event.Events.Transfiguration2Spec (spec) where

import Arkham.ClassSymbol
import Arkham.Classes.HasGame (getGame)
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Projection
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
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

  -- #5448: "You have 1 fewer hand slots" is part of Marion Tavares's card front, so
  -- Transfiguration copies it along with her skill values and abilities.
  it "treats the front of your investigator card as the chosen card (slots)" . gameTest $ \self -> do
    handSlots self `shouldReturn` 2
    self `playEvent` Events.transfiguration2
    chooseMarionTavares
    handSlots self `shouldReturn` 1
    run $ EndOfGame Nothing
    handSlots self `shouldReturn` 2

  it "gives back a slot the investigator's own front takes away"
    . gameTestWith Investigators.marionTavares
    $ \self -> do
      handSlots self `shouldReturn` 1
      self `playEvent` Events.transfiguration2
      chooseHankSamson
      handSlots self `shouldReturn` 2

  -- #5544: the class symbol is printed on the front too, but InvestigatorClass was
  -- the one printed value with no TransfiguredForm branch, so a transfigured
  -- investigator still reported their original class to InvestigatorWithClass,
  -- DifferentClassAmong and SearchCollectionForRandom.
  it "treats the front of your investigator card as the chosen card (class)" . gameTest $ \self -> do
    -- Jenny Barnes is a Rogue, Hank Samson is a Survivor
    field InvestigatorClass self.id `shouldReturn` Rogue
    self `playEvent` Events.transfiguration2
    chooseHankSamson
    field InvestigatorClass self.id `shouldReturn` Survivor
    run $ EndOfGame Nothing
    field InvestigatorClass self.id `shouldReturn` Rogue

  -- The attrs keep the original class so the form can be dropped again, so the
  -- transfigured class has to be applied on the wire as well or the client themes
  -- the investigator by their original class while showing the chosen card's front.
  it "publishes the transfigured class" . gameTest $ \self -> do
    self `playEvent` Events.transfiguration2
    chooseHankSamson
    publishedClass `shouldReturn` Just (toJSON Survivor)
    run $ EndOfGame Nothing
    publishedClass `shouldReturn` Just (toJSON Rogue)

  it "only lasts until the end of the game" . gameTest $ \self -> do
    self `playEvent` Events.transfiguration2
    chooseHankSamson
    self.combat `shouldReturn` 5
    run $ EndOfGame Nothing
    self.willpower `shouldReturn` 3
    self.intellect `shouldReturn` 3
    self.combat `shouldReturn` 3
    self.agility `shouldReturn` 3

  -- #5316: a Body of a Yithian stores a snapshot of the investigator it was made from.
  -- Transfiguration (2) used to overwrite that snapshot with the Yithian's own attrs on
  -- the very next message, so The City of Archives could never give the mind back and
  -- the game died with "the original mind of the Yithian is lost".
  it "does not destroy the original body of a Body of a Yithian" . gameTestWith Investigators.rolandBanks $ \self -> do
    run $ BecomeYithian (toId self)
    originalBody <- yithianOriginalCardCode <$> getInvestigator (toId self)
    originalBody `shouldBe` Just "01001"

    self `playEvent` Events.transfiguration2
    chooseHankSamson
    -- any message resolved while transfigured used to re-seed the snapshot
    run $ SetInvestigatorForm (toId self) RegularForm
    stillOriginalBody <- yithianOriginalCardCode <$> getInvestigator (toId self)
    stillOriginalBody `shouldBe` Just "01001"

    run ResetInvestigators
    attrs <- toAttrs <$> getInvestigator (toId self)
    investigatorCardCode attrs `shouldBe` "01001"
    investigatorCombat attrs `shouldBe` 4

offeredInvestigators :: TestAppT [CardCode]
offeredInvestigators = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, ChooseOne msgs)] -> pure [code | CardLabel code _ _ <- msgs]
    q -> error $ "expected a single ChooseOne, got: " <> show q

chooseHankSamson :: TestAppT ()
chooseHankSamson = chooseInvestigatorCard "Hank Samson" "10015"

chooseMarionTavares :: TestAppT ()
chooseMarionTavares = chooseInvestigatorCard "Marion Tavares" "11001"

chooseInvestigatorCard :: String -> CardCode -> TestAppT ()
chooseInvestigatorCard name cCode = chooseOptionMatching ("choose " <> name) \case
  CardLabel code _ _ -> code == cCode
  _ -> False

handSlots :: Investigator -> TestAppT Int
handSlots self = length . findWithDefault [] #hand <$> self.slots

-- The class the client actually receives, read off the PublicGame encoding. The
-- harness runs a single investigator, so take the only one published.
publishedClass :: TestAppT (Maybe Value)
publishedClass = do
  game <- getGame
  let
    obj k (Object o) = KeyMap.lookup (Key.fromText k) o
    obj _ _ = Nothing
  pure $ case obj "investigators" (toJSON (PublicGame (1 :: Int) "test" [] game)) of
    Just (Object invs) -> case KeyMap.elems invs of
      [i] -> obj "class" i
      _ -> Nothing
    _ -> Nothing
