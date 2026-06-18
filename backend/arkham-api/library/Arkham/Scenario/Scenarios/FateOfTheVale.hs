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
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers (draw, unDeck)
import Arkham.Helpers.Act (getCurrentAct, getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays, withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWith)
import Arkham.Helpers.Query (allInvestigators, getSetAsideCardMaybe)
import Arkham.Helpers.SkillTest (isEvadeWith, isFightWith)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Id
import Arkham.Investigator.Cards qualified as InvestigatorCards
import Arkham.Investigator.Types (Field (..))
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationCardsUnderneath, LocationLabel))
import Arkham.Matcher hiding (Discarded, enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, remember)
import Arkham.Message.Lifted.Move
import Arkham.Modifier (UIModifier (..), setActiveDuringSetup)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck (ScenarioDeckKey (..))
import Arkham.Scenario.Import.Lifted hiding (say, (.=))
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.ScenarioLogKey (ScenarioLogKey (BertieIsFleeing))
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Strategy (FoundCardsStrategy (..), ZoneReturnStrategy (..), fromDeck, fromDiscard)
import Arkham.Token (Token (Resource))
import Arkham.Trait (Trait (Cave, Colour, Emissary))
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
  [ ". . caveTop1 caveTop2 . ."
  , ". . . mirrorNestTop . ."
  , "caveLeft1 mirrorNestLeft cosmicEmissaryPhantasm cosmicEmissaryAbyss . ."
  , "caveLeft2 . cosmicEmissaryBrilliance cosmicEmissaryMiasma mirrorNestRight caveRight1"
  , ". . mirrorNestBottom . . caveRight2"
  , ". . caveBottom1 caveBottom2 . ."
  ]

cosmicEmissaryCaveLabels :: [(Text, [Text])]
cosmicEmissaryCaveLabels =
  [ ("mirrorNestTop", ["caveTop1", "caveTop2"])
  , ("mirrorNestRight", ["caveRight1", "caveRight2"])
  , ("mirrorNestBottom", ["caveBottom1", "caveBottom2"])
  , ("mirrorNestLeft", ["caveLeft1", "caveLeft2"])
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

openCaveLabelFor :: ReverseQueue m => LocationId -> m (Maybe Text)
openCaveLabelFor nest = do
  nestLabel <- field LocationLabel nest
  occupied <- selectField LocationLabel Anywhere
  let openForNest = find (`notElem` occupied) =<< lookup nestLabel cosmicEmissaryCaveLabels
  case openForNest of
    Just label -> pure (Just label)
    Nothing -> pure $ find (`notElem` occupied) (concatMap snd cosmicEmissaryCaveLabels)

{- | Draw a card from The Abyss. The card leaves The Abyss immediately and the
actual resolution is routed through a cancellable window so cards such as Old
Memory can react before it resolves (see 'resolveAbyssDraw').
-}
drawCardFromAbyss :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
drawCardFromAbyss iid source card = do
  scenarioSpecific "removeFromAbyss" (toCardId card)
  abyssDrawWindow "scenario" iid card
    $ ScenarioSpecific "resolveAbyssDraw" (toJSON (iid, source, card))

{- | Resolve a card drawn from The Abyss. Resident cards have special text on
The Abyss: flip them to their enemy side, they attack without engaging, then
are discarded instead of going to hand.
-}
resolveAbyssDraw :: ReverseQueue m => InvestigatorId -> Source -> Card -> m ()
resolveAbyssDraw iid source card = do
  if toCardType card == LocationType
    then do
      let mirrorNest = LocationWithTitle "Mirror Nest"
      preferred <-
        select
          $ NearestLocationTo iid
          $ mirrorNest
          <> not_ (ConnectedTo NotForMovement (LocationWithTrait Cave))
      fallback <- select $ NearestLocationTo iid mirrorNest
      let choices = if null preferred then fallback else preferred
      let
        placeAndConnect :: ReverseQueue m => LocationId -> m ()
        placeAndConnect nest = do
          lid <- placeLocation card
          openCaveLabelFor nest >>= traverse_ (setLocationLabel lid)
          connectBothWays nest lid
      case choices of
        [] -> void $ placeLocation card
        [nest] -> placeAndConnect nest
        nests -> chooseTargetM iid nests placeAndConnect
    else case residentFromCardDef (toCardDef card) of
      Just resident -> do
        enemyCard <- fetchCard (residentEnemyDef resident)
        focusCards [enemyCard] do
          chooseOneM iid do
            labeled "Continue" do
              unfocusCards
              obtainCard card
              withLocationOf iid \lid -> do
                eid <- createEnemyAt enemyCard lid
                initiateEnemyAttack eid source iid
                toDiscard source eid
      Nothing -> case card of
        PlayerCard _ -> addToHand iid [card]
        _ -> drawCard iid card

instance HasModifiersFor FateOfTheVale where
  getModifiersFor (FateOfTheVale attrs) = do
    modifySelectWith
      attrs
      (enemyIsExact Enemies.cosmicEmissaryThePhantasm)
      setActiveDuringSetup
      [UIModifier $ Rotated 90]
    modifySelectWith
      attrs
      (enemyIsExact Enemies.cosmicEmissaryTheMiasma)
      setActiveDuringSetup
      [UIModifier $ Rotated 270]
    modifySelectWith
      attrs
      (assetIs Assets.drRosaMarquezBestInHerField)
      setActiveDuringSetup
      [DoNotTakeUpSlot #ally]
    -- Once The Abyss is a location on the map, the encounter deck is gone, so the
    -- mythos draw must be initiated by clicking The Abyss instead of the (absent)
    -- encounter deck. The draw still routes through The Silence into the Abyss deck.
    when (getMetaKeyDefault "abyssIsLocation" False attrs)
      $ modifySelect
        attrs
        Anyone
        [DrawEncounterCardsVia $ LocationTargetMatches $ locationIs Locations.theAbyssSpiralingOblivion]

instance HasChaosTokenValue FateOfTheVale where
  getChaosTokenValue iid tokenFace (FateOfTheVale attrs) = case tokenFace of
    Skull -> do
      actStep <- getCurrentActStep
      colourEnemies <- selectCount $ EnemyWithTrait Colour
      pure $ toChaosTokenValue attrs Skull (actStep + 1) colourEnemies
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> pure $ toChaosTokenValue attrs Tablet 5 8
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FateOfTheVale where
  runMessage msg s@(FateOfTheVale attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
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
        , Locations.theAbyssSpiralingOblivion
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
          Just def -> setOwner iid =<< genCard def
      shuffledDeck <- shuffle $ map EncounterCard (unDeck encounterDeck) <> playerCards
      let (bottomHalf, topHalf) = splitAt (length shuffledDeck `div` 2) shuffledDeck
      topHalfWithTrueSelves <- shuffle $ trueInvestigatorCards <> topHalf
      attrsL . encounterDeckL .= mempty
      attrsL . decksL %= insertMap AbyssDeck (topHalfWithTrueSelves <> bottomHalf)
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      randomDiscard iid Cultist
      pure s
    ResolveChaosToken drawnToken Tablet iid -> do
      chooseOneM iid do
        when (isEasyStandard attrs) $ labeled' "tablet.easyStandard" do
          placeDoom Tablet iid 1
          chaosTokenEffect Tablet drawnToken $ ChaosTokenFaceModifier [Zero]
        when (isHardExpert attrs) $ labeled' "tablet.hardExpert" do
          placeDoom Tablet iid 1
          chaosTokenEffect Tablet drawnToken $ ChaosTokenFaceModifier [MinusThree]
        unscoped skip_
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      let cosmicEmissary = EnemyWithTrait Emissary
      againstEmissary <- (||) <$> isFightWith cosmicEmissary <*> isEvadeWith cosmicEmissary
      when againstEmissary
        $ if isEasyStandard attrs then drawAnotherChaosToken iid else failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Cultist
      , isEasyStandard attrs -> do
          randomDiscard iid Cultist
          pure s
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
      let isAct2 = actStep == 2
      let isAct3 = actStep == 3
      actCard <- field ActCard =<< getCurrentAct
      let isVersion def = toCardCode actCard == toCardCode def
      case n of
        2 -> scope "leahAtwood" do
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate (not isAct2) "act3"
          when isAct2 do
            -- Search all out-of-play areas for an Item asset and draw it.
            codexFinishedUntilNewAct 2
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
          when isAct3 do
            codexFinished 2
            eachInvestigator \iid' -> chooseOneM iid' do
              labeled' "drawTwo" $ drawCards iid' source 2
              labeled' "healHorror" $ healHorror iid' source 1
        3 -> scope "simeonAtwood" do
          let isFateOfTheValeV3 = isAct3 && isVersion Acts.fateOfTheValeV3
              isOtherwise = not isAct2 && not isFateOfTheValeV3
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate isFateOfTheValeV3 "act3"
              hr
              p.validate isOtherwise "otherwise"
          if isAct2
            then do
              codexFinishedUntilNewAct 3
              selectEach (EnemyWithTrait Emissary) (automaticallyEvadeEnemy iid)
            else do
              codexFinished 3
              if isFateOfTheValeV3
                then do
                  locs <- select Anywhere
                  chooseTargetM iid locs \lid -> placeTokens source lid Resource 3
                else do
                  eachInvestigator \iid' -> chooseOneM iid' do
                    labeled' "drawOne" $ drawCards iid' source 1
                    labeled' "gainResources" $ gainResources iid' source 2
        4 -> scope "williamHemlock" do
          let isFateOfTheValeV2 = isAct3 && isVersion Acts.fateOfTheValeV2
              isOtherwise = not isAct2 && not isFateOfTheValeV2
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate isFateOfTheValeV2 "act3"
              hr
              p.validate isOtherwise "otherwise"
          if isAct2
            then do
              codexFinishedUntilNewAct 4
              eachInvestigator \iid' -> gainClues iid' source 1
            else do
              codexFinished 4
              if isFateOfTheValeV2
                then drawResidentUnderneath iid source
                else do
                  eachInvestigator \iid' -> chooseOneM iid' do
                    labeled' "drawNone" nothing
                    labeled' "drawOne" $ drawCards iid' source 1
                    labeled' "drawTwo" $ drawCards iid' source 2
        5 -> scope "riverHawthorne" do
          let isFateOfTheValeV2 = isAct3 && isVersion Acts.fateOfTheValeV2
              isOtherwise = not isAct2 && not isFateOfTheValeV2
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate isFateOfTheValeV2 "act3"
              hr
              p.validate isOtherwise "otherwise"
          if isAct2
            then do
              codexFinishedUntilNewAct 5
              -- Play an Item card from your hand, ignoring all costs.
              search iid source iid [(FromHand, PutBack)] (basic #item) (PlayFoundNoCost iid 1)
            else do
              codexFinished 5
              if isFateOfTheValeV2
                then drawResidentUnderneath iid source
                else gainResources iid source 4
        6 -> scope "gideonMizrah" do
          let isFateOfTheValeV1 = isAct3 && isVersion Acts.fateOfTheValeV1
              isOtherwise = not isAct2 && not isFateOfTheValeV1
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate isFateOfTheValeV1 "act3"
              hr
              p.validate isOtherwise "otherwise"
          if isAct2
            then do
              codexFinishedUntilNewAct 6
              eachInvestigator \iid' ->
                nextTurnModifier iid' source iid' (AdditionalActions "Gideon Mizrah" source 1)
            else do
              codexFinished 6
              if isFateOfTheValeV1
                then do
                  selectEach (EnemyWithTrait Emissary <> enemyAtLocationWith iid) (automaticallyEvadeEnemy iid)
                else do
                  selectJust AnyAgenda >>= \agenda -> removeDoom source agenda 1
        7 -> scope "judithPark" do
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate (not isAct2) "act3"
          if isAct2
            then do
              codexFinishedUntilNewAct 7
              enemies <- select AnyEnemy
              chooseTargetM iid enemies (automaticallyEvadeEnemy iid)
            else do
              codexFinished 7
              -- Search your deck, hand, and discard pile for a Weapon and play it, ignoring all costs.
              search
                iid
                source
                iid
                [fromDeck, (FromHand, PutBack), fromDiscard]
                (basic #weapon)
                (PlayFoundNoCost iid 1)
        8 -> scope "theoPeters" do
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct2 "act2"
              hr
              p.validate (not isAct2) "otherwise"
          if isAct2
            then do
              codexFinishedUntilNewAct 8
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
              codexFinished 8
              selectEach (enemyEngagedWith iid) (disengageEnemy iid)
              locs <- select Anywhere
              chooseTargetM iid locs (moveTo source iid)
        Theta -> scope "drRosaMarquez" do
          let isAct1 = actStep == 1
              isFateOfTheValeV1 = isAct3 && isVersion Acts.fateOfTheValeV1
              isOtherwise = not isAct1 && not isFateOfTheValeV1
          flavor do
            setTitle "title"
            compose.green do
              p.validate isAct1 "act1"
              hr
              p.validate isFateOfTheValeV1 "act3"
              hr
              p.validate isOtherwise "otherwise"
          if isAct1
            then do
              codexFinishedUntilNewAct Theta
              -- Reveal the bottom 6 cards of The Abyss; you may draw any of them.
              let abyss = findWithDefault [] AbyssDeck attrs.decks
              let bottom6 = drop (max 0 (length abyss - 6)) abyss
              unless (null bottom6) $ focusCards bottom6 do
                chooseOneAtATimeM iid do
                  labeled' "doneDrawing" unfocusCards
                  targets bottom6 $ drawCardFromAbyss iid source
            else do
              if isOtherwise
                then codexFinishedUntilNewAct Theta
                else codexFinished Theta
              eachInvestigator \iid' -> gainClues iid' source 1
        Omega -> scope "bertieMusgrave" do
          codexFinished Omega
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
    AddToHand iid [card]
      | any ((== toCardId card) . toCardId) (findWithDefault [] AbyssDeck attrs.decks)
      , Just _ <- residentFromCardDef (toCardDef card) -> do
          drawCardFromAbyss iid ScenarioSource card
          pure s
    ScenarioSpecific "drawFromAbyss" v -> do
      let (iid, card) = toResult v :: (InvestigatorId, Card)
      drawCardFromAbyss iid ScenarioSource card
      pure s
    DoBatch _ (ScenarioSpecific "resolveAbyssDraw" v) -> do
      let (iid, source, card) = toResult v :: (InvestigatorId, Source, Card)
      resolveAbyssDraw iid source card
      pure s
    ScenarioSpecific "theAbyssBecameLocation" _ -> do
      pure $ FateOfTheVale $ attrs & setMetaKey "abyssIsLocation" True
    ScenarioSpecific "removeFromAbyss" v -> do
      let cid = toResult v :: CardId
      pure $ FateOfTheVale $ attrs & decksL . at AbyssDeck %~ fmap (filter ((/= cid) . toCardId))
    _ -> FateOfTheVale <$> liftRunMessage msg attrs
