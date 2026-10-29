module Arkham.Game.CollectionSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Game.Diff qualified as GameDiff
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Message.Lifted.Choose (chooseCollectionCards, collectionCardDefs)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Entity.Answer (Answer (..), QuestionResponse (..), Reply (..), handleAnswerPure)
import TestImport.New

spec :: Spec
spec = describe "collection card pool" do
  it
    "includes only this player's library and the game's recorded custom cards, with aliases deduplicated"
    . gameTest
    $ \_ -> do
      g <- getGame
      let
        own = CustomCard (emptyCardDef "*collection-own0" "Own skill" SkillType) Nothing
        recorded = CustomCard (emptyCardDef "*collection-recorded0" "Recorded skill" SkillType) Nothing
        private = CustomCard (emptyCardDef "*collection-private0" "Private skill" SkillType) Nothing
        encounter = CustomCard (emptyCardDef "*collection-enemy0" "Encounter enemy" EnemyType) Nothing
        library =
          mapFromList
            [("*collection-own0", own), ("*collection-alias0", own), ("*collection-enemy0", encounter)]
        scoped =
          g
            { gameCollectionCards = Transient (Just library)
            , gameCustomCards = singletonMap "*collection-recorded0" recorded
            }
      registerCustomCards $ singletonMap "*collection-private0" private
      defs <- runReaderT collectionCardDefs scoped
      let codes = map toCardCode defs
      codes `shouldSatisfy` elem "01039"
      length (filter (== "*collection-own0") codes) `shouldBe` 1
      codes `shouldSatisfy` elem "*collection-recorded0"
      codes `shouldSatisfy` notElem "*collection-private0"
      codes `shouldSatisfy` notElem "*collection-enemy0"
      Aeson.toJSON scoped `shouldBe` Aeson.toJSON (scoped {gameCollectionCards = Transient Nothing})

  it "stores definitions outside compact questions and validates choices by index" . gameTest $ \self -> do
    let
      code = "*compact-collection0"
      def =
        (emptyCardDef code "Custom skill" SkillType)
          { cdLevel = Just 0
          , cdMeta = singletonMap "largeScript" (toJSON $ T.replicate 50000 "x")
          }
      custom = CustomCard def Nothing
      source = TestSource mempty
    overTest \g -> g {gameCollectionCards = Transient $ Just $ singletonMap code custom}
    runQueueT $ chooseCollectionCards self.id source [def, def]
    runMessages
    current <- getGame
    pid <- getPlayer self.id
    let question = ChooseFromCollection self.id source [code]
    lookup pid (gameQuestion current) `shouldBe` Just question
    lookup code (gameCustomCards current) `shouldBe` Just custom
    length (Aeson.encode question) `shouldSatisfy` (< 500)
    Aeson.toJSON (GameDiff.diff current (current {gameCustomCards = mempty}))
      `shouldBe` Aeson.toJSON ([] :: [Aeson.Value])
    restored <- case Aeson.fromJSON (Aeson.toJSON current) of
      Aeson.Error err -> error err
      Aeson.Success g -> pure g
    lookup code (gameCustomCards restored) `shouldBe` Just custom
    liftIO (handleAnswerPure restored pid (Answer $ QuestionResponse 0 (Just pid) Nothing)) >>= \case
      Handled msgs -> msgs `shouldSatisfy` elem (HandleTargetChoice self.id source $ CardCodeTarget code)
      Unhandled err -> expectationFailure $ show err
    for_ [-1, 1] \choiceIndex ->
      liftIO (handleAnswerPure restored pid (Answer $ QuestionResponse choiceIndex (Just pid) Nothing)) >>= \case
        Handled msgs -> do
          msgs `shouldSatisfy` elem (Ask pid question)
          msgs `shouldSatisfy` notElem (HandleTargetChoice self.id source $ CardCodeTarget code)
        Unhandled err -> expectationFailure $ show err
    let legacy =
          restored
            { gameQuestion =
                singletonMap pid
                  $ ChooseOne [CardLabel code False [HandleTargetChoice self.id source $ CardCodeTarget code]]
            }
    liftIO (handleAnswerPure legacy pid (Answer $ QuestionResponse 0 (Just pid) Nothing)) >>= \case
      Handled msgs -> msgs `shouldSatisfy` elem (Run [HandleTargetChoice self.id source $ CardCodeTarget code])
      Unhandled err -> expectationFailure $ show err

  for_ [Locations.secretsLongForgotten_193, Locations.secretsLongForgotten_194] \def ->
    it ("defers the collection search until horror is assigned: " <> show (toCardCode def)) . gameTest $ \self -> do
      location <- testLocationWithDef def id
      let ability = UseCardAbility self.id (toSource location) 1 [] NoPayment
      run ability
      g <- getGame
      let searchOptions =
            [ messages
            | q <- toList (gameQuestion g)
            , ChooseOne options <- [stripQuestionWrappers q]
            , Label label messages <- options
            , "searchForASkill" `T.isSuffixOf` label
            ]
      case searchOptions of
        [messages] -> do
          length messages `shouldBe` 2
          messages `shouldSatisfy` elem (DoStep 1 ability)
        _ -> expectationFailure "expected a horror-and-search option"
      chooseOptionMatching "take horror and search" \case
        Label label _ -> "searchForASkill" `T.isSuffixOf` label
        _ -> False
      self.horror `shouldReturn` 0
      chooseOnlyOption "assign the horror"
      self.horror `shouldReturn` 1
      afterHorror <- getGame
      let collectionChoices =
            [ options
            | q <- toList (gameQuestion afterHorror)
            , ChooseOne options <- [stripQuestionWrappers q]
            ]
      collectionChoices `shouldSatisfy` \case
        [options] -> notNull options && all (\case CardLabel {} -> True; _ -> False) options
        _ -> False
