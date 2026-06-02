module Arkham.Scenario.Scenarios.FateOfTheVale (fateOfTheVale) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types (finalizeDraw)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers (draw, unDeck)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Investigator.Types (Field (..))
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (Discarded, enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier (UIModifier (..), setActiveDuringSetup)
import Arkham.Projection
import Arkham.Scenario.Deck (ScenarioDeckKey (..))
import Arkham.Scenario.Import.Lifted hiding ((.=))
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.Story.Cards qualified as Stories
import Control.Lens (use, (%=), (.=))

newtype FateOfTheVale = FateOfTheVale ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheVale :: Difficulty -> FateOfTheVale
fateOfTheVale difficulty = scenario FateOfTheVale "10651" "Fate of the Vale" difficulty []

cosmicEmissaryFormation :: [(Text, CardDef, CardDef)]
cosmicEmissaryFormation =
  [ ("mirrorNestTop", Enemies.cosmicEmissaryTheAbyss, Locations.mirrorNest10666)
  , ("mirrorNestRight", Enemies.cosmicEmissaryTheMiasma, Locations.mirrorNest10667)
  , ("mirrorNestBottom", Enemies.cosmicEmissaryTheBrilliance, Locations.mirrorNest10668)
  , ("mirrorNestLeft", Enemies.cosmicEmissaryThePhantasm, Locations.mirrorNest10669)
  ]

cosmicEmissaryLayout :: [GridTemplateRow]
cosmicEmissaryLayout =
  [ ". . mirrorNestTop ."
  , "mirrorNestLeft cosmicEmissaryPhantasm cosmicEmissaryAbyss ."
  , ". cosmicEmissaryBrilliance cosmicEmissaryMiasma mirrorNestRight"
  , ". mirrorNestBottom . ."
  ]

instance HasModifiersFor FateOfTheVale where
  getModifiersFor (FateOfTheVale attrs) = do
    modifySelectWith
      attrs
      (mapOneOf enemyIs [Enemies.cosmicEmissaryThePhantasm, Enemies.cosmicEmissaryThePhantasmShattered])
      setActiveDuringSetup
      [UIModifier $ Rotated 90]
    modifySelectWith
      attrs
      (mapOneOf enemyIs [Enemies.cosmicEmissaryTheMiasma, Enemies.cosmicEmissaryTheMiasmaShattered])
      setActiveDuringSetup
      [UIModifier $ Rotated 270]

instance HasChaosTokenValue FateOfTheVale where
  getChaosTokenValue iid tokenFace (FateOfTheVale attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FateOfTheVale where
  runMessage msg s@(FateOfTheVale attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    Setup -> runScenarioSetup (FateOfTheVale . (encounterDeckL .~ mempty)) attrs do
      setup $ ul do
        li "gatherSets"
        li "nightThree"
        li "shatteredSelf"
        li.nested "mirrorNests" do
          li "shuffleMirrorNests"
          li "connections"
          li "startingLocation"
        li.nested "setAside" do
          li "removeScenarioReference"
        li.nested "createTheAbyss" do
          li "seedAbyssDeck"
          li "shuffleTrueSelves"
          li "placeTheAbyss"
        unscoped $ li "readyToBegin"

      scope "theAbyss" $ flavor $ setTitle "title" >> p "body"
      scope "cosmicEmissary" $ flavor $ setTitle "title" >> p "body"
      scope "shatteredSelf" $ flavor $ setTitle "title" >> p "body"

      gather Set.TheFinalDay
      gather Set.FateOfTheVale
      gather Set.AgentsOfTheColour
      gather Set.Refractions
      gather Set.Transfiguration
      gather Set.TheVale
      gather Set.HorrorsInTheRock

      gatherAndSetAside Set.DayOfTheFeast
      gatherAndSetAside Set.Residents
      gatherAndSetAside Set.Fire

      placeStory Stories.nightThree
      setScenarioDayAndTime

      setAgendaDeck [Agendas.theSilence, Agendas.theMiasma, Agendas.theSpiral]
      setActDeck [Acts.shatteredMemories, Acts.lostSelf]

      eachInvestigator $ push . BecomeShatteredSelf

      mirrorNests <- shuffle $ map (\(_, _, nest) -> nest) cosmicEmissaryFormation

      setLayout cosmicEmissaryLayout
      nestLids <- for (zip cosmicEmissaryFormation mirrorNests) \((label, emissary, _), nest) -> do
        lid <- placeLabeled label nest
        void $ enemyAt emissary lid
        pure lid

      for_ (zip nestLids (drop 1 nestLids <> take 1 nestLids)) (uncurry connectBothWays)

      eachInvestigator \iid -> chooseTargetM iid nestLids $ moveTo_ attrs iid

      setAside
        [ Acts.fateOfTheValeV1
        , Acts.fateOfTheValeV2
        , Acts.fateOfTheValeV3
        , Acts.fateOfTheValeV4
        , Stories.theAbyss
        ]
      setAsideEvery (CardFromEncounterSet Set.TheVale <> #location)
      setAsideEvery (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      setAsideEvery (cardIs Enemies.crystalParasite)

      encounterDeck <- use (attrsL . encounterDeckL)
      investigators <- allInvestigators
      playerCards <-
        concat <$> for investigators \iid -> do
          (cards, rest) <- fieldMap InvestigatorDeck (draw 5) iid
          push $ LoadDeck iid rest
          pure $ map PlayerCard cards
      trueInvestigatorCards <- for investigators \iid -> do
        cardCode <- field InvestigatorCardCode iid
        case lookup cardCode InvestigatorCards.allInvestigatorCards of
          Nothing -> error $ "Could not find investigator card: " <> show cardCode
          Just def -> genCard def
      shuffledDeck <- shuffle $ map EncounterCard (unDeck encounterDeck) <> playerCards
      let (bottomHalf, topHalf) = splitAt (length shuffledDeck `div` 2) shuffledDeck
      topHalfWithTrueSelves <- shuffle $ trueInvestigatorCards <> topHalf
      attrsL . encounterDeckL .= mempty
      attrsL . decksL %= insertMap AbyssDeck (topHalfWithTrueSelves <> bottomHalf)
    Do (DrawCards iid drawing) | drawing.deck == Deck.EncounterDeck -> do
      let
        drawOneFromAbyss deck = do
          let
            go rv [] = (Nothing, [], rv)
            go rv (card : rest) = case card of
              EncounterCard ec -> (Just ec, reverse rest, rv)
              _ -> go (card : rv) rest
            (mEncounter, remaining, revealed) = go [] (reverse deck)
          shuffledRevealed <- shuffle revealed
          pure (mEncounter, shuffledRevealed <> remaining)

        drawFromAbyss 0 deck drawn = pure (reverse drawn, deck)
        drawFromAbyss n deck drawn = do
          (mEncounter, deck') <- drawOneFromAbyss deck
          case mEncounter of
            Nothing -> pure (reverse drawn, deck')
            Just encounter -> drawFromAbyss (n - 1) deck' (EncounterCard encounter : drawn)

      let abyssDeck = findWithDefault [] AbyssDeck attrs.decks
      (drawn, abyssDeck') <- drawFromAbyss drawing.amount abyssDeck []
      pushWhen (notNull drawn) $ DrewCards iid $ finalizeDraw drawing $ drawing.alreadyDrawn <> drawn
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck ?~ abyssDeck'
    ShuffleCardsIntoDeck Deck.EncounterDeck cards -> do
      shuffled <- shuffle $ cards <> findWithDefault [] AbyssDeck attrs.decks
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck ?~ shuffled
    ShuffleCardsIntoTopOfDeck Deck.EncounterDeck n cards -> do
      let (topCards, rest) = splitAt n $ findWithDefault [] AbyssDeck attrs.decks
      shuffled <- shuffle $ cards <> topCards
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck ?~ (shuffled <> rest)
    PutCardOnTopOfDeck _ Deck.EncounterDeck card -> do
      pure
        $ FateOfTheVale
        $ attrs
        & decksL
        . at AbyssDeck
        %~ Just
        . (card :)
        . filter (/= card)
        . fromMaybe []
    PutCardOnBottomOfDeck _ Deck.EncounterDeck card -> do
      pure
        $ FateOfTheVale
        $ attrs
        & decksL
        . at AbyssDeck
        %~ Just
        . (<> [card])
        . filter (/= card)
        . fromMaybe []
    AddToEncounterDiscard ec -> do
      quietCancelCardDraw (EncounterCard ec)
      pushAll [ObtainCard (toCardId ec), Do msg]
      pure s
    Do (AddToEncounterDiscard ec) -> do
      pure
        $ FateOfTheVale
        $ attrs
        & decksL
        . at AbyssDeck
        %~ Just
        . (EncounterCard ec :)
        . filter (/= EncounterCard ec)
        . fromMaybe []
    AddToSpecificEncounterDiscard _ ec -> do
      quietCancelCardDraw (EncounterCard ec)
      pushAll [ObtainCard (toCardId ec), Do msg]
      pure s
    Do (AddToSpecificEncounterDiscard _ ec) -> do
      pure
        $ FateOfTheVale
        $ attrs
        & decksL
        . at AbyssDeck
        %~ Just
        . (EncounterCard ec :)
        . filter (/= EncounterCard ec)
        . fromMaybe []
    Discarded _ _ (EncounterCard ec) -> do
      pure
        $ FateOfTheVale
        $ attrs
        & decksL
        . at AbyssDeck
        %~ Just
        . (EncounterCard ec :)
        . filter (/= EncounterCard ec)
        . fromMaybe []
    InvestigatorDrewEncounterCard _ ec -> do
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck %~ fmap (filter (/= EncounterCard ec))
    InvestigatorDrewEncounterCardFrom _ ec _ -> do
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck %~ fmap (filter (/= EncounterCard ec))
    _ -> FateOfTheVale <$> liftRunMessage msg attrs
