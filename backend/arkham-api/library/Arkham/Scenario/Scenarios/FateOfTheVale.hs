module Arkham.Scenario.Scenarios.FateOfTheVale (fateOfTheVale) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (ActCard))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types (finalizeDraw)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers (draw, unDeck)
import Arkham.Helpers.Act (getCurrentAct, getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (allInvestigators, getSetAsideCardMaybe)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Id
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Investigator.Types (Field (..))
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher hiding (Discarded, enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, remember)
import Arkham.Message.Lifted.Move
import Arkham.Modifier (UIModifier (..), setActiveDuringSetup)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck (ScenarioDeckKey (..))
import Arkham.Scenario.Import.Lifted hiding ((.=), say)
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.ScenarioLogKey (ScenarioLogKey (BertieIsFleeing))
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy (FoundCardsStrategy (..), ZoneReturnStrategy (..), fromDeck, fromDiscard)
import Arkham.Token (Token (Resource))
import Arkham.Trait (Trait (Emissary))
import Arkham.Zone (OutOfPlayZone (..), Zone (..))
import Control.Lens (use, (%=), (.=))

newtype FateOfTheVale = FateOfTheVale ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheVale :: Difficulty -> FateOfTheVale
fateOfTheVale difficulty = scenario FateOfTheVale "10651" "Fate of the Vale" difficulty []

cosmicEmissaryFormation :: [(Text, CardDef, CardDef)]
cosmicEmissaryFormation =
  [ ("mirrorNestTop", Enemies.cosmicEmissaryTheAbyss, Locations.mirrorNest_166)
  , ("mirrorNestRight", Enemies.cosmicEmissaryTheMiasma, Locations.mirrorNest_167)
  , ("mirrorNestBottom", Enemies.cosmicEmissaryTheBrilliance, Locations.mirrorNest_168)
  , ("mirrorNestLeft", Enemies.cosmicEmissaryThePhantasm, Locations.mirrorNest_169)
  ]

cosmicEmissaryLayout :: [GridTemplateRow]
cosmicEmissaryLayout =
  [ ". . mirrorNestTop ."
  , "mirrorNestLeft cosmicEmissaryPhantasm cosmicEmissaryAbyss ."
  , ". cosmicEmissaryBrilliance cosmicEmissaryMiasma mirrorNestRight"
  , ". mirrorNestBottom . ."
  ]

{- | Cross out the name of each resident that was not under control of an
investigator at the end of the game.
-}
crossOutUncontrolledResidents :: ReverseQueue m => m ()
crossOutUncontrolledResidents =
  for_ [minBound .. maxBound] \resident -> do
    controlled <- selectAny (assetIs resident <> AssetControlledBy Anyone)
    unless controlled $ record (crossedOutKey resident)

{- | Cross out the name of each resident that was in the victory display at the
end of the game.
-}
crossOutResidentsInVictoryDisplay :: ReverseQueue m => m ()
crossOutResidentsInVictoryDisplay = do
  victoryDisplay <- scenarioField ScenarioVictoryDisplay
  for_ [minBound .. maxBound] \resident ->
    when (any ((== toCardCode resident) . toCardCode) victoryDisplay)
      $ record (crossedOutKey resident)

{- | The enemy-side card def for each resident. Residents are double-sided; the
asset side and enemy side are separate cards in the engine.
-}
residentEnemyDef :: Resident -> CardDef
residentEnemyDef = \case
  WilliamHemlock -> Enemies.williamHemlock
  RiverHawthorne -> Enemies.riverHawthorne
  MotherRachel -> Enemies.motherRachelStarbornHerald
  SimeonAtwood -> Enemies.simeonAtwood
  LeahAtwood -> Enemies.leahAtwood
  TheoPeters -> Enemies.theoPeters
  GideonMizrah -> Enemies.gideonMizrah
  JudithPark -> Enemies.judithPark

{- | "Draw a Resident card underneath any location and put it into play at that
location, exhausted, enemy side faceup." (Codex 4/Codex 5, Act 3a v.II)
-}
drawResidentUnderneath :: ReverseQueue m => InvestigatorId -> Source -> m ()
drawResidentUnderneath iid source = do
  locs <- select Anywhere
  xs <- concatForM locs \lid -> do
    under <- field LocationCardsUnderneath lid
    pure [(lid, c, r) | c <- under, Just r <- [residentFromCardDef (toCardDef c)]]
  unless (null xs) $ chooseOneM iid $ for_ xs \(lid, c, r) ->
    cardLabeled c do
      obtainCard c
      eid <- createEnemyAt (residentEnemyDef r) lid
      exhaustEnemy source eid

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
    ScenarioResolution res -> scope "resolutions" do
      case res of
        NoResolution -> do
          resolution "noResolution"
          record TheInvestigatorsBecameTheTrueFeastOfHemlockVale
          eachInvestigator $ kill attrs
          gameOver
        Resolution 1 -> do
          crossOutUncontrolledResidents
          record DrMarquezSacrificedHerselfForTheVale
          eachInvestigator \iid -> do
            inDeck <- selectAny $ inDeckOf iid <> basic (cardIs Assets.drRosaMarquezBestInHerField)
            when inDeck $ removeCampaignCardFromDeck iid Assets.drRosaMarquezBestInHerField
          eachInvestigator \iid -> sufferTrauma iid 1 1
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs (toBonus "cosmicEmissary" 5)
          endOfScenarioThen EpilogueStep
        Resolution 2 -> do
          record TheInvestigatorsSacrificedThemselvesForTheVale
          resolution "resolution2"
          eachInvestigator $ kill attrs
          endOfScenarioThen EpilogueStep
        Resolution 3 -> do
          crossOutResidentsInVictoryDisplay
          record TheValeWasSaved
          eachInvestigator \iid -> sufferTrauma iid 2 2
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs (toBonus "savedTheVale" 3)
          endOfScenarioThen EpilogueStep
        Resolution 4 -> do
          crossOutUncontrolledResidents
          record TheValeBurned
          eachInvestigator \iid -> sufferTrauma iid 2 2
          resolutionWithXp "resolution4" $ allGainXpWithBonus' attrs (toBonus "eradicatedTheColour" 3)
          endOfScenarioThen EpilogueStep
        Resolution 5 -> do
          crossOutUncontrolledResidents
          record TheInvestigatorsBarelySurvivedTheFeastOfHemlockVale
          eachInvestigator \iid -> sufferTrauma iid 3 3
          resolutionWithXp "resolution5" $ allGainXpWithBonus' attrs (toBonus "survivedTheFeast" 2)
          endOfScenarioThen EpilogueStep
        _ -> error "invalid resolution"
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      actStep <- getCurrentActStep
      actCard <- field ActCard =<< getCurrentAct
      let isVersion def = toCardCode actCard == toCardCode def
      let say sub = scope sub $ flavor $ setTitle "title" >> p.green "body"
      case n of
        2 -> scope "leahAtwood" do
          if actStep == 2
            then do
              say "act2"
              -- Search all out-of-play areas for an Item asset and draw it.
              search
                iid
                source
                iid
                [ (FromOutOfPlay SetAsideZone, PutBack)
                , (FromOutOfPlay VictoryDisplayZone, PutBack)
                , (FromOutOfPlay VoidZone, PutBack)
                ]
                (basic (#item <> #asset))
                (DrawFound iid 1)
            else do
              say "act3"
              eachInvestigator \iid' -> chooseOneM iid' do
                labeled' "drawTwo" $ drawCards iid' source 2
                labeled' "healHorror" $ healHorror iid' source 1
        3 -> scope "simeonAtwood" do
          if actStep == 2
            then do
              say "act2"
              selectEach (EnemyWithTrait Emissary) (automaticallyEvadeEnemy iid)
            else
              if isVersion Acts.fateOfTheValeV3
                then do
                  say "fateOfTheValeV3"
                  locs <- select Anywhere
                  chooseTargetM iid locs \lid -> placeTokens source lid Resource 3
                else do
                  say "otherwise"
                  eachInvestigator \iid' -> chooseOneM iid' do
                    labeled' "drawOne" $ drawCards iid' source 1
                    labeled' "gainResources" $ gainResources iid' source 2
        4 -> scope "williamHemlock" do
          if actStep == 2
            then do
              say "act2"
              eachInvestigator \iid' -> gainClues iid' source 1
            else
              if isVersion Acts.fateOfTheValeV2
                then say "fateOfTheValeV2" >> drawResidentUnderneath iid source
                else do
                  say "otherwise"
                  eachInvestigator \iid' -> chooseOneM iid' do
                    labeled' "drawNone" nothing
                    labeled' "drawOne" $ drawCards iid' source 1
                    labeled' "drawTwo" $ drawCards iid' source 2
        5 -> scope "riverHawthorne" do
          if actStep == 2
            then do
              say "act2"
              -- Play an Item card from your hand, ignoring all costs.
              search iid source iid [(FromHand, PutBack)] (basic #item) (PlayFoundNoCost iid 1)
            else
              if isVersion Acts.fateOfTheValeV2
                then say "fateOfTheValeV2" >> drawResidentUnderneath iid source
                else say "otherwise" >> gainResources iid source 4
        6 -> scope "gideonMizrah" do
          if actStep == 2
            then do
              say "act2"
              eachInvestigator \iid' ->
                nextTurnModifier iid' source iid' (AdditionalActions "Gideon Mizrah" source 1)
            else
              if isVersion Acts.fateOfTheValeV1
                then do
                  say "fateOfTheValeV1"
                  selectEach (EnemyWithTrait Emissary <> enemyAtLocationWith iid) (automaticallyEvadeEnemy iid)
                else do
                  say "otherwise"
                  selectJust AnyAgenda >>= \agenda -> removeDoom source agenda 1
        7 -> scope "judithPark" do
          if actStep == 2
            then do
              say "act2"
              enemies <- select AnyEnemy
              chooseTargetM iid enemies (automaticallyEvadeEnemy iid)
            else do
              say "act3"
              -- Search your deck, hand, and discard pile for a Weapon and play it, ignoring all costs.
              search
                iid
                source
                iid
                [fromDeck, (FromHand, PutBack), fromDiscard]
                (basic #weapon)
                (PlayFoundNoCost iid 1)
        8 -> scope "theoPeters" do
          if actStep == 2
            then do
              say "act2"
              -- Search The Abyss for a location, put it into play, and move to it.
              let abyss = findWithDefault [] AbyssDeck attrs.decks
              let locationCards = filter ((== LocationType) . toCardType) abyss
              unless (null locationCards) $ focusCards locationCards do
                chooseOneM iid do
                  unscoped $ labeled' "skip" unfocusCards
                  targets locationCards \card -> do
                    unfocusCards
                    lid <- placeLocation card
                    moveTo source iid lid
                    scenarioSpecific "removeFromAbyss" (toCardId card)
            else do
              say "otherwise"
              selectEach (enemyEngagedWith iid) (disengageEnemy iid)
              locs <- select Anywhere
              chooseTargetM iid locs (moveTo source iid)
        Theta -> scope "drRosaMarquez" do
          if actStep == 1
            then do
              say "act1"
              -- Reveal the bottom 6 cards of The Abyss; you may draw any of them.
              let abyss = findWithDefault [] AbyssDeck attrs.decks
              let bottom6 = drop (max 0 (length abyss - 6)) abyss
              unless (null bottom6) $ focusCards bottom6 do
                chooseOneAtATimeM iid do
                  labeled' "doneDrawing" unfocusCards
                  targets bottom6 \card -> do
                    addToHand iid [card]
                    scenarioSpecific "removeFromAbyss" (toCardId card)
            else do
              if isVersion Acts.fateOfTheValeV1 then say "fateOfTheValeV1" else say "otherwise"
              eachInvestigator \iid' -> gainClues iid' source 1
        Omega -> scope "bertieMusgrave" do
          flavor $ setTitle "title" >> p.green "body"
          selectOne (assetIs Assets.bertieMusgraveATrueAesthete) >>= \case
            Just aid -> takeControlOfAsset iid aid
            Nothing -> do
              selectEach (enemyIs Enemies.bertieMusgrave) removeFromGame
              getSetAsideCardMaybe Assets.bertieMusgraveATrueAesthete
                >>= traverse_ (takeControlOfSetAsideAsset iid)
          when (isVersion Acts.fateOfTheValeV4) $ remember BertieIsFleeing
        _ -> pure ()
      pure s
    ScenarioSpecific "removeFromAbyss" v -> do
      let cid = toResult v :: CardId
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck %~ fmap (filter ((/= cid) . toCardId))
    _ -> FateOfTheVale <$> liftRunMessage msg attrs
