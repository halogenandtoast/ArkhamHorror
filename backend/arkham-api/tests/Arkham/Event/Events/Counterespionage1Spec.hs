module Arkham.Event.Events.Counterespionage1Spec (spec) where

import Arkham.Ability.Types
import Arkham.Classes.HasGame (getGame)
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Counterespionage (1)" $ do
  context "when you drew the treachery" $ do
    -- Solo: the "any investigator" +2 has nobody to redirect to, so only the
    -- "your deck" +2 is offered, and it stays optional.
    it "leaves the cost increase optional" . gameTest $ \self -> do
      setup self self
      choices <- choicesFor self
      abilityIndices choices `shouldBe` [1]
      any isSkip choices `shouldBe` True

  context "when you drew the treachery and another investigator is in play" $ do
    it "leaves both cost increases optional" . gameTest $ \self -> do
      void $ addInvestigator rolandBanks
      setup self self
      choices <- choicesFor self
      abilityIndices choices `shouldBe` [1, 2]
      any isSkip choices `shouldBe` True

  context "when another investigator drew the treachery" $ do
    -- The +2 to change "you" to "any investigator" is the only thing that makes
    -- the play legal, so it is forced and there must be no way to skip it.
    it "forces the +2 that changes \"you\" to \"any investigator\"" . gameTest $ \self -> do
      other <- addInvestigator rolandBanks
      setup self other
      -- getActions offers forced abilities alone, ahead of the optional ones
      choices <- choicesFor self
      abilityIndices choices `shouldBe` [2]
      any isSkip choices `shouldBe` False

      -- and only once it has resolved does the optional "your deck" +2 appear
      chooseOnlyOption "forced cost increase"
      choices' <- choicesFor self
      abilityIndices choices' `shouldBe` [1]
      any isSkip choices' `shouldBe` True

setup :: Investigator -> Investigator -> TestAppT ()
setup self drawer = do
  counterespionage <- genCard Events.counterespionage1
  frozenInFear <- genEncounterCard Treacheries.frozenInFear
  -- NOTE: We generate another card here to prevent the deck from shuffling
  ancientEvils <- genEncounterCard Treacheries.ancientEvils
  withProp @"resources" 10 self
  withProp @"hand" [counterespionage] self
  run $ SetEncounterDeck (Deck [frozenInFear, ancientEvils])
  run $ drawEncounterCard drawer.id GameSource
  chooseTarget counterespionage

{- | The choices currently offered to an investigator, with the display-only
question wrappers peeled off.
-}
choicesFor :: HasCallStack => Investigator -> TestAppT [UI Message]
choicesFor i = do
  pid <- getPlayer (toId i)
  questionMap <- gameQuestion <$> getGame
  case lookup pid questionMap of
    Nothing -> error "no question for that investigator"
    Just q -> case stripQuestionWrappers q of
      ChooseOne msgs -> pure msgs
      PlayerWindowChooseOne msgs -> pure msgs
      other -> error $ "expected a ChooseOne, got " <> show other

{- | Asserting on the indices (rather than a count) keeps the skip assertions
from passing vacuously and names which ability went missing when they don't.
-}
abilityIndices :: [UI Message] -> [Int]
abilityIndices choices = sort [abilityIndex ability | AbilityLabel {ability} <- choices]

isSkip :: UI Message -> Bool
isSkip = \case
  SkipTriggersButton {} -> True
  _ -> False
