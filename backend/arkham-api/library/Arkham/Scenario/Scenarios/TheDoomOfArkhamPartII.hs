module Arkham.Scenario.Scenarios.TheDoomOfArkhamPartII (theDoomOfArkhamPartII) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Name (nameTitle, toName)
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck (ScenarioDeckKey (CthulhuDeck))
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Trait (Trait (Artifact))

newtype TheDoomOfArkhamPartII = TheDoomOfArkhamPartII ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

instance HasModifiersFor TheDoomOfArkhamPartII where
  getModifiersFor (TheDoomOfArkhamPartII a) =
    modifySelectWith a (mapOneOf assetIs allies) setActiveDuringSetup [DoNotTakeUpSlot #ally]

{- FOURMOLU_DISABLE -}
theDoomOfArkhamPartII :: Difficulty -> TheDoomOfArkhamPartII
theDoomOfArkhamPartII difficulty =
  scenario
    TheDoomOfArkhamPartII
    "11688a"
    "The Doom of Arkham Pt II"
    difficulty
    [ ".               northside            downtown  easttown"
    , "westernRooftops miskatonicUniversity rivertown easternRooftops"
    , ".               stMarysHospital      southside ."
    ]
{- FOURMOLU_ENABLE -}

allies :: [CardDef]
allies = [Assets.johnRaymondLegrasse, Assets.rubyStandish, Assets.andyVanNortwick]

instance HasChaosTokenValue TheDoomOfArkhamPartII where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfArkhamPartII attrs) = case chaosTokenFace of
    Skull -> do
      rage <- getCthulhuRage
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs rage (rage + 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 1 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDoomOfArkhamPartII where
  runMessage msg s@(TheDoomOfArkhamPartII attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      artifacts <- length <$> getUncrossedArtifacts
      let canRitual = artifacts >= 5
      storyWithChooseOneM'
        ( do
            setTitle "title"
            p "theDoomOfArkham1"
            p.basic "mustDecide"
            ul do
              li "lastStand"
              li.validate canRitual "anotherWay"
        )
        do
          labeled' "lastStand" $ doStep 2 msg
          labeledValidate' canRitual "anotherWay" $ doStep 3 msg
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor do
        setTitle "title"
        p "theDoomOfArkham2"
        ul do
          li "recordStoodTogether"
          li "alliesIntoPlay"
          li "addToken"
        p.basic "proceedToSetup"
      record TheInvestigatorsStoodTogether
      addChaosToken ElderThing
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor do
        setTitle "title"
        p "theDoomOfArkham3"
        p "theDoomOfArkham3Continued"
        p "theDoomOfArkham3Ruby"
        ul do
          li "recordAlliesHaveAPlan"
          li "alliesSetAside"
          li "addToken"
        p.basic "proceedToSetup"
      record YourAlliesHaveAPlan
      addChaosToken ElderThing
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDoomOfArkhamPartII attrs do
      stoodTogether <- getHasRecord TheInvestigatorsStoodTogether
      setup $ ul do
        li.nested "gatherSets" $ li "midnightMasksLocations"
        li.nested "placeLocations" do
          li "arkhamLocations"
          li "rivertownRuined"
          li "setAsideLocations"
          li "cthulhuAtRivertown"
          li "startAtRivertown"
        li.nested "cthulhuBoard" do
          li "placeBoard"
          li "placeFacets"
        li "buildCthulhuDeck"
        li.nested "checkCampaignLogActAgenda" do
          li.validate stoodTogether "fightBack"
          li.validate (not stoodTogether) "banishHim"
        li.nested "checkCampaignLog" do
          li "artifacts"
          li "floodedNeighborhoods"
        li "starSpawnAside"
        li "cthulhusRage"
        li "buildEncounterDeck"
        li "readyToBegin"

      additionalRules "cthulhuBoard"
      additionalRules "cthulhuDeck"
      additionalRules "cthulhusRage"

      gather Set.TheDoomOfArkhamPartII
      gather Set.Domination
      gather Set.ElderMist
      gather Set.Flood
      gather Set.StarSpawn
      gather Set.AgentsOfCthulhu

      gatherJustMatching Set.TheMidnightMasks #location

      setActDeck [if stoodTogether then Acts.fightBack else Acts.banishHim]
      setAgendaDeck [Agendas.theDoomOfArkham]

      unless stoodTogether do
        setAside [Agendas.theFinalSeal]

      addExtraDeck CthulhuDeck
        =<< shuffleM
        =<< amongGathered (CardFromEncounterSet Set.TheDoomOfArkhamPartII <> #story)

      removeCards =<< amongGathered (CardFromEncounterSet Set.TheMidnightMasks <> #location)
      setAside [Locations.graveyard, Locations.yourHouse]

      placeOneOf_ (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      placeOneOf_ (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      placeAll
        [ Locations.northside
        , Locations.easttown
        , Locations.stMarysHospital
        , Locations.miskatonicUniversity
        , Locations.westernRooftops
        , Locations.easternRooftops
        ]
      rivertown <- place Locations.rivertownTheDrownedCity
      startAt rivertown

      createEnemyAt_ Enemies.cthulhuAncientEvil rivertown
      for_ cthulhuBoardSlots \(_, facet) -> createEnemyAt_ facet rivertown

      -- "Set the Star Spawn encounter set aside, out of play." The agenda shuffles
      -- them back in one at a time.
      setAsideEvery (CardFromEncounterSet Set.StarSpawn)

      -- "Place 1 resource on the scenario reference card, under 'Cthulhu's Rage'. If
      -- there are only 1 or 2 players, place 1 additional resource."
      playerCount <- getPlayerCount
      increaseCthulhuRage (if playerCount <= 2 then 2 else 1)

      -- "Increase the flood level of each location listed under 'Flooded
      -- Neighborhoods'." Part I recorded the card codes, but Rivertown comes back
      -- Ruined here, so the entries are matched on title rather than code.
      floodedTitles <- map (nameTitle . toName) . mapMaybe lookupCardDef <$> getFloodedNeighborhoods
      for_ floodedTitles \title ->
        selectOne (LocationWithTitle title) >>= traverse_ increaseFloodLevel

      eachInvestigator (`forInvestigator` Setup)
      doStep 1 Setup
    ForInvestigator _ Setup -> pure s
    DoStep 1 Setup -> do
      -- "If the investigators stood together", the three allies join them; on the
      -- ritual route they wait out of play until Banish Him! hands them out.
      stoodTogether <- getHasRecord TheInvestigatorsStoodTogether
      lead <- getLead
      for_ allies \ally -> do
        card <- genPlayerCard ally
        if stoodTogether
          then do
            investigators <- select Anyone
            chooseOrRunOneM lead do
              questionLabeled' "chooseAllyInvestigator"
              targets investigators \iid -> createAssetAt_ (toCard card) (InPlayArea iid)
          else push $ SetAsideCards [toCard card]
      -- "Gather all earned artifacts that are not crossed out ... Put each of them
      -- into play under an investigator's control, divided as evenly as possible."
      artifacts <- getUncrossedArtifacts
      for_ artifacts \def -> push $ ForTarget (CardCodeTarget def.cardCode) (DoStep 2 Setup)
      pure s
    ForTarget (CardCodeTarget cardCode) (DoStep 2 Setup) -> do
      for_ (lookupCardDef cardCode) \def -> do
        investigators <- select Anyone
        counts <-
          for investigators \iid ->
            (iid,)
              <$> selectCount (AssetWithTrait Artifact <> AssetControlledBy (InvestigatorWithId iid))
        unless (null counts) do
          let fewest = minimumEx $ map snd counts
          card <- EncounterCard <$> genEncounterCard def
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled' "chooseArtifactInvestigator"
            targets [iid | (iid, n) <- counts, n == fewest] (createAssetAt_ card . InPlayArea)
      pure s
    {- "When drawing cards from the Cthulhu deck, resolve each effect, one at a time,
    from top to bottom. After resolving an action card, it is discarded to the
    Cthulhu discard pile." Action cards are stories, not treacheries, so they are
    read rather than drawn into anyone's hand. -}
    DrewCards iid drew | drew.deck == Deck.ScenarioDeckByKey CthulhuDeck -> do
      for_ drew.cards \card -> do
        resolveStory iid card
        push $ ScenarioSpecific "discardCthulhuCard" (toJSON card)
      pure s
    ScenarioSpecific "discardCthulhuCard" (toResult -> card) ->
      pure $ TheDoomOfArkhamPartII $ attrs & deckDiscardsL %~ insertWith (<>) CthulhuDeck [card]
    ScenarioResolution res -> scope "resolutions" do
      case res of
        Resolution 1 -> do
          record CthulhuWasDrivenAway
          resolutionWithXp "resolution1" $ allGainXp' attrs
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 3
            sufferMentalTrauma iid 3
        Resolution 2 -> do
          record CthulhuWasBanished
          resolutionWithXp "resolution2" $ allGainXp' attrs
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 1
            sufferMentalTrauma iid 1
            removeRandomBasicWeakness iid
        Resolution 3 -> do
          record CthulhuWasBanished
          record ArkhamWasDestroyed
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs $ toBonus "bonus" 5
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 2
            sufferMentalTrauma iid 2
        NoResolution -> do
          record CthulhuAnnihilatedTheCityOfArkham
          resolution "noResolution"
          -- "The investigators, along with everyone else in Arkham, are killed."
          eachInvestigator (kill attrs)
          gameOver
        _ -> error $ "Unknown resolution: " <> show res
      -- Every ending but the total loss carries on to the campaign's epilogue.
      unless (res == NoResolution) $ endOfScenarioThen EpilogueStep
      pure s
    _ -> TheDoomOfArkhamPartII <$> liftRunMessage msg attrs

{- | "Each investigator may remove 1 random basic weakness from their deck (ignoring
deckbuilding requirements) for saving Arkham from an ancient evil."
-}
removeRandomBasicWeakness :: (HasI18n, ReverseQueue m) => InvestigatorId -> m ()
removeRandomBasicWeakness iid = do
  weaknesses <- select $ basic BasicWeaknessCard <> InDeckOf (InvestigatorWithId iid)
  unless (null weaknesses) do
    chooseOneM iid do
      labeled' "removeNoWeakness" nothing
      cardsLabeled weaknesses $ removeCardFromDeckForCampaign iid
