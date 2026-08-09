module Arkham.Scenario.Scenarios.TheBlobThatAteEverything (theBlobThatAteEverything) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option (CampaignOption (PlayWithTheBlobThatAteEverythingElse))
import Arkham.Card (Card (..), lookupCardDef)
import Arkham.Card.CardCode (CardCode)
import Arkham.Card.EncounterCard (lookupEncounterCard)
import Arkham.Card.Id (unsafeMakeCardId)
import Arkham.DamageEffect (damageAssignmentAmount, nonAttack)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Epic.Types (
  GroupOrdinal (..),
  SharedKey (Countermeasures, ReplicationPending),
  groupOrdinalKey,
  sharedKeyText,
 )
import Arkham.Helpers.Enemy (getModifiedKeywords)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Query (allInvestigators, getLead, getPlayerCount)
import Arkham.Helpers.SkillTest (
  getCommittedCards,
  getSkillTestAction,
  getSkillTestTargetedEnemy,
  withSkillTest,
 )
import Arkham.Helpers.Xp (toBonus)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (PhaseStep)
import Arkham.Message.Lifted.Choose
import Arkham.Phase
import Arkham.Placement (Placement (AtLocation, AttachedToEnemy, Global, InThreatArea))
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioTokens), difficultyL)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (TreacheryPlacement))
import Data.Aeson.Key qualified as Key

newtype TheBlobThatAteEverything = TheBlobThatAteEverything ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theBlobThatAteEverything :: Difficulty -> TheBlobThatAteEverything
theBlobThatAteEverything difficulty =
  sideStory TheBlobThatAteEverything "85001" "The Blob That Ate Everything" difficulty []

instance HasChaosTokenValue TheBlobThatAteEverything where
  getChaosTokenValue iid face (TheBlobThatAteEverything attrs) = case face of
    Skull -> do
      devoured <- getDevouredCount
      let divisor = if isEasyStandard attrs then 5 else 3
      pure $ ChaosTokenValue Skull (NegativeModifier $ devoured `div` divisor)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 7
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheBlobThatAteEverything where
  runMessage msg s@(TheBlobThatAteEverything attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    HandleOption PlayWithTheBlobThatAteEverythingElse ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "blobThatAteEverythingElse" (True :: Bool)
    Setup -> do
      -- Epic Multiplayer group games are flagged in scenario meta at creation
      -- (see Api.Handler.Arkham.Events.createGroupGame): the join/setup path has
      -- no event context, so we cannot consult the event row here. In epic mode
      -- the global health pool (Subject 8L-08) and the countermeasures pool are
      -- event-wide shared state, so we place the epic subject and let the shared
      -- pool seed/reconcile the countermeasures rather than placing local tokens.
      isEpic <- getScenarioMetaKeyDefault "epicMultiplayer" False
      isElse <- getScenarioMetaKeyDefault "blobThatAteEverythingElse" False
      runScenarioSetup TheBlobThatAteEverything attrs do
        setup $ ul do
          li "gatherSets"
          li.validate isElse "gatherElseSets"
          li.validate isEpic "epicMultiplayer"
          li.validate (not isEpic) "singleGroup"
          li "setAsideMiGo"
          li "setAside"
          li "subject"
          li.nested "placeLocations" do
            li "shuffleQuarantine"
            li "placeCrater"
            li "innerRing"
            li "outerRing"
            li "remaining"
            li "startAt"
          li.validate (not isEpic) "countermeasures"
          unscoped $ li "shuffleRemainder"

        setUsesGrid

        gather Set.TheBlobThatAteEverything
        gatherAndSetAside Set.MiGoIncursion
        when isElse $ gatherAndSetAside Set.MiGoIncursionII
        -- Reward assets are single-sided player cards (no encounter back), so
        -- they live in the player pool and are no longer pulled in by the
        -- encounter-set gather above; set them aside explicitly for the stories
        -- that grant them (Recover the Sample, Rescue the Chemist, etc.).
        setAside
          [ Assets.universalSolvent
          , Assets.petOozeling
          , Assets.miGoWeapon
          , Assets.ltWilsonStewart
          ]
        when isElse
          $ setAside
            [Assets.armoredCar, Assets.brainCase, Assets.gMen, Assets.corrosiveCloud, Assets.alienInstruments]

        placeEnemy (if isEpic then Enemies.subject8L08EpicMultiplayer else Enemies.subject8L08) Global

        setAside
          [ Enemies.vulnerableHeart
          , Enemies.graspingOoze
          , Enemies.cubicOoze
          , Enemies.oozewraith
          , Enemies.oozewraith
          ]

        setAgendaDeck [Agendas.theAnomalySpreads, Agendas.theAnomalySwells, Agendas.theAnomalyConsumes]
        -- Epic Multiplayer uses Act 1 and Act 3 variants whose clue thresholds are
        -- a single GLOBAL pool (2 per investigator across all groups); Act 2 is
        -- shared with the single-group deck.
        setActDeck
          $ if isEpic
            then
              [ Acts.exposeTheAnomalyEpicMultiplayer
              , Acts.extraterrestrialPhysiology
              , Acts.blackwatersBaneEpicMultiplayer
              ]
            else [Acts.exposeTheAnomaly, Acts.extraterrestrialPhysiology, Acts.blackwatersBane]

        quarantine <-
          shuffle
            $ [ Locations.sewer
              , Locations.bridge
              , Locations.waterTower
              , Locations.church
              , Locations.oozyLakebed
              , Locations.oozyLakebed
              , Locations.slimyStreets
              , Locations.slimyStreets
              , Locations.desiccatedFarmland
              , Locations.desiccatedFarmland
              ]
            <> if isElse
              then
                [ Locations.mainStreet
                , Locations.oldBurialHill
                , Locations.abandonedWindmill
                , Locations.abbatoir
                ]
              else []

        let
          quarantine' = drop (if isElse then 5 else 1) quarantine
          (innerQuarantine, rest1) = splitAt 2 quarantine'
          (outerQuarantine, remainingQuarantine) = splitAt 3 rest1

        placeInGrid_ (Pos 0 0) Locations.theCrater

        innerDefs <-
          shuffle
            $ Locations.researchSiteTheBlobThatAteEverything
            : Locations.temporaryHQ
            : innerQuarantine
        innerIds <- for (zip [Pos 0 1, Pos 0 (-1), Pos 1 0, Pos (-1) 0] innerDefs) \(pos, def) -> do
          lid <- placeInGrid pos def
          pure (def, lid)

        outerDefs <- shuffle $ Locations.fungusMound : outerQuarantine
        for_ (zip [Pos 0 2, Pos 0 (-2), Pos 2 0, Pos (-2) 0] outerDefs) (uncurry placeInGrid_)

        for_
          (zip [Pos 1 1, Pos 1 (-1), Pos (-1) 1, Pos (-1) (-1)] remainingQuarantine)
          (uncurry placeInGrid_)

        for_ (lookup Locations.temporaryHQ innerIds) startAt

        -- Single Group seeds local countermeasures here; in Epic Multiplayer the
        -- countermeasures pool is event-wide shared state (seeded at event
        -- creation = ceil(total/2)) and is reconciled into this group's scenario
        -- Resource tokens at the start of each action, so skip the local seed.
        unless isEpic do
          playerCount <- getPlayerCount
          placeTokens ScenarioSource ScenarioTarget #resource (if playerCount >= 3 then 2 else 1)
    SetChaosTokensForScenario -> do
      setChaosTokens
        $ if isEasyStandard attrs
          then
            [ PlusOne
            , Zero
            , Zero
            , Zero
            , MinusOne
            , MinusTwo
            , MinusTwo
            , MinusThree
            , MinusFour
            , MinusFive
            , Skull
            , Skull
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
          else
            [ Zero
            , Zero
            , Zero
            , MinusOne
            , MinusOne
            , MinusTwo
            , MinusThree
            , MinusFour
            , MinusFive
            , MinusSix
            , Skull
            , Skull
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
      pure s
    ResolveChaosToken _ Cultist _ -> do
      -- If revealed during an attack against an enemy, that enemy gains
      -- retaliate for this attack.
      whenM ((== Just #fight) <$> getSkillTestAction) do
        whenJustM getSkillTestTargetedEnemy \enemy ->
          withSkillTest \sid -> skillTestModifier sid Cultist enemy (AddKeyword Keyword.Retaliate)
      pure s
    ResolveChaosToken _ Tablet _ -> do
      -- After this skill test ends, Subject 8L-08 devours each committed card.
      afterSkillTestQuiet do
        iids <- allInvestigators
        cards <- concatMapM getCommittedCards iids
        devour cards
      pure s
    ResolveChaosToken _token ElderThing iid -> do
      -- Choose up to N cards from your hand. Subject 8L-08 devours each of them.
      -- You get +1 skill value for this test for each card devoured this way.
      withSkillTest \sid -> do
        let n = if isEasyStandard attrs then 5 else 7
        hand <- field InvestigatorHand iid
        chooseUpToNM iid n "doneDevouring" do
          for_ hand \card -> cardLabeled card do
            devour [card]
            skillTestModifier sid ElderThing iid (AnySkillValue 1)
      pure s
    Damaged (EnemyTarget eid) assignment -> do
      foodAndDrinksActive <- getScenarioMetaKeyDefault "foodAndDrinksActive" False
      isSubject <- eid <=~> subject8L08Matcher
      if foodAndDrinksActive && isSubject
        then do
          damageDealt <- getScenarioMetaKeyDefault "foodAndDrinksDamageDealt" 0
          let damageDealt' = damageDealt + damageAssignmentAmount assignment
          pure
            $ TheBlobThatAteEverything
            $ attrs
            & setMetaKey "foodAndDrinksDamageDealt" (damageDealt' :: Int)
            & setMetaKey "foodAndDrinksActive" (damageDealt' < 3)
        else pure s
    Defeated (EnemyTarget eid) _ _ _ -> do
      modifiers <- getModifiers eid
      unless (ScenarioModifier "noBlob" `elem` modifiers) do
        keywords <- getModifiedKeywords eid
        let blobX = [x | Keyword.ScenarioKeywordX "Blob" x <- toList keywords]
        for_ (listToMaybe blobX) \x -> do
          let extra = sum [n | ScenarioModifierValue "Blob" (maybeResult -> Just n) <- modifiers]
          if ScenarioModifier "oldBurialHillBlob" `elem` modifiers
            then
              selectOne (locationIs Locations.oldBurialHill) >>= traverse_ \loc -> do
                lead <- getLead
                discoverAt NotInvestigate lead (toSource attrs) (x + extra) loc
            else do
              subject <- selectJust subject8L08Matcher
              push $ DealDamage (EnemyTarget subject) (nonAttack Nothing attrs (x + extra))
      pure s
    AdvanceAgendaBy _ _ ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "senseOfTimeActive" (False :: Bool)
    PhaseStep (InvestigationPhaseStep InvestigationPhaseEndsStep) _ ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "languageActive" (False :: Bool)
    EndRound ->
      pure
        $ TheBlobThatAteEverything
        $ attrs
        & setMetaKey "lightActive" (False :: Bool)
        & setMetaKey "lightDevoured" (False :: Bool)
        & setMetaKey "voiceActive" ([] :: [InvestigatorId])
        & setMetaKey "friendshipsActive" (False :: Bool)
    -- Reality Acid records which one-time "aspects of reality" have already
    -- been devoured (per investigator or per group) in the scenario meta.
    ScenarioSpecific "blobSetMeta" (maybeResult -> Just (key, value)) -> do
      let
        setLightDevoured = if key == "lightActive" then setMetaKey "lightDevoured" True else id
        setFoodAndDrinksActive =
          if key == "foodAndDrinks"
            then setMetaKey "foodAndDrinksActive" True . setMetaKey "foodAndDrinksDamageDealt" (0 :: Int)
            else id
      pure
        $ TheBlobThatAteEverything
        $ attrs
        & setMetaKey (Key.fromText key) (value :: Value)
        & setLightDevoured
        & setFoodAndDrinksActive
    -- The organizer nominates a Replicating Aberration and a legal in-group
    -- target. Investigators get the printed countermeasure cancellation choice;
    -- the actual spawn is a second scenario message so the choice remains a
    -- normal, persisted game question.
    ScenarioSpecific "blobMainStreetReady" (maybeResult -> Just iid) ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "mainStreetReady" (iid :: InvestigatorId)
    ScenarioSpecific "blobRequestAberration" payload
      | Just (cardCode, target) <- maybeResult payload -> do
          countermeasures <- scenarioFieldMap ScenarioTokens (Token.countTokens Token.Resource)
          lead <- getLead
          ordinal <- scenarioCount (EpicShared groupOrdinalKey)
          let
            pendingKey = ReplicationPending $ GroupOrdinal ordinal
            spawn = ScenarioSpecific "blobSpawnAberration" (toJSON (cardCode :: CardCode, target :: Target))
          push $ RaiseShared pendingKey 1
          if countermeasures > 0
            then chooseOneM lead do
              labeled "Spend 1 countermeasure to prevent the replication" do
                push $ RemoveTokens (toSource attrs) ScenarioTarget Token.Resource 1
                push $ SpendShared pendingKey 1
              labeled "Allow the Replicating Aberration to spawn" do
                push $ SpendShared pendingKey 1
                push spawn
            else do
              push $ SpendShared pendingKey 1
              push spawn
          pure s
    ScenarioSpecific "blobSpawnAberration" payload
      | Just (cardCode, target) <- maybeResult payload -> do
          cardDef <-
            maybe (error "Unknown Replicating Aberration card code") pure $ lookupCardDef (cardCode :: CardCode)
          cardId <- unsafeMakeCardId <$> getRandom
          let card = EncounterCard $ lookupEncounterCard cardDef cardId
          placement <- case target :: Target of
            LocationTarget lid -> pure $ AtLocation lid
            InvestigatorTarget iid -> pure $ InThreatArea iid
            EnemyTarget eid -> do
              placement' <- field EnemyPlacement eid
              when (cardCode == "89010a") do
                lead <- getLead
                push $ Discard (Just lead) (toSource attrs) (EnemyTarget eid)
              pure placement'
            _ -> error "Invalid Replicating Aberration target"
          eid <- case placement of
            AtLocation lid -> createEnemyAt card lid
            InThreatArea iid -> do
              lid <- fieldMap InvestigatorLocation (fromJustNote "investigator must be at a location") iid
              eid <- createEnemyAt card lid
              engageEnemy iid eid
              pure eid
            _ -> error "Replicating Aberration target is not in play"
          exhaustEnemy (toSource attrs) eid
          when (cardCode == "89010f") $ createTreacheryAt_ Treacheries.alienFoodChain (AttachedToEnemy eid)
          when (cardCode == "89010h") $ case target of
            EnemyTarget spottedEnemy -> do
              foodChains <- select $ treacheryIs Treacheries.alienFoodChain
              for_ foodChains \foodChain -> whenM (fieldMap TreacheryPlacement (== AttachedToEnemy spottedEnemy) foodChain) do
                push $ PlaceTreachery foodChain (AttachedToEnemy eid)
            _ -> pure ()
          pure s
    ScenarioSpecific "blobSetLightActive" (maybeResult -> Just active) ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "lightActive" (active :: Bool)
    ScenarioSpecific "blobSetDebugRealityAcidTokens" value -> do
      let tokens = maybeResult value :: Maybe [ChaosTokenFace]
      case tokens of
        Just tokens' ->
          pure $ TheBlobThatAteEverything $ attrs & setMetaKey "debugRealityAcidTokens" tokens'
        Nothing -> pure s
    ScenarioSpecific "blobClearDebugRealityAcidTokens" _ ->
      pure
        $ TheBlobThatAteEverything
        $ attrs
        & setMetaKey "debugRealityAcidTokens" ([] :: [ChaosTokenFace])
    -- Track every card exiled during the scenario so Reality Acid can devour
    -- "all cards that have been exiled".
    Exiled _ card -> do
      exiled <- getScenarioMetaKeyDefault "exiledCards" []
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "exiledCards" (card : exiled :: [Card])
    -- Reality Acid's "concept of easiness": flip the scenario reference card to
    -- its Hard/Expert side.
    ScenarioSpecific "blobFlipToHard" _ -> do
      let harder = case attrs ^. difficultyL of
            Easy -> Hard
            Standard -> Expert
            d -> d
      pure $ TheBlobThatAteEverything $ attrs & difficultyL .~ harder
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution 1) -> scope "resolutions" do
      resolution "resolution1"
      push GameOver
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> scope "resolutions" do
      resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 3
      -- Any one investigator may add each in-play reward asset to their deck.
      rewardAssets <-
        select
          $ mapOneOf
            assetIs
            [Assets.universalSolvent, Assets.petOozeling, Assets.miGoWeapon, Assets.ltWilsonStewart]
      for_ rewardAssets addCampaignCardToDeckChoice_
      endOfScenario
      pure s
    -- Epic Multiplayer: countermeasures are an event-wide shared pool. At the
    -- start of each action the authoritative value is mirrored in as the
    -- EpicShared "countermeasures" count; reconcile this group's local Resource
    -- tokens to it so the existing token-cost gain/spend UI and cost checks see
    -- the global count. (The EpicShared count is also stored generically below.)
    ScenarioCountSet (EpicShared key) v | key == sharedKeyText Countermeasures -> do
      let attrs' = attrs & tokensL %~ Token.setTokens Token.Resource (max 0 v)
      TheBlobThatAteEverything <$> liftRunMessage msg attrs'
    -- Countermeasures are scenario Resource tokens on ScenarioTarget. Every gain
    -- (Research Site) and every spend (Research Site / The Crater / Fungus Mound /
    -- Temporary HQ / Reality Acid) flows through these two messages, so hooking
    -- here propagates the change to the shared pool from a single place. Single
    -- Group games (no epic flag) are unaffected.
    PlaceTokens _ ScenarioTarget Token.Resource n -> do
      whenM (getScenarioMetaKeyDefault "epicMultiplayer" False) $ push $ RaiseShared Countermeasures n
      TheBlobThatAteEverything <$> liftRunMessage msg attrs
    RemoveTokens _ ScenarioTarget Token.Resource n -> do
      whenM (getScenarioMetaKeyDefault "epicMultiplayer" False) $ push $ SpendShared Countermeasures n
      TheBlobThatAteEverything <$> liftRunMessage msg attrs
    _ -> TheBlobThatAteEverything <$> liftRunMessage msg attrs
