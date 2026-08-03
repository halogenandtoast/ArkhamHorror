module Arkham.Scenario.Scenarios.TheDoomOfArkhamPartI (theDoomOfArkhamPartI) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheDoomOfArkhamPartII)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card
import Arkham.Card.PlayerCard (setPlayerCardOwner)
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationCard))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers
import Arkham.Trait (Trait (Injury, Madness))

newtype TheDoomOfArkhamPartI = TheDoomOfArkhamPartI ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

{- FOURMOLU_DISABLE -}
{- | The Arkham locations keep their Midnight Masks arrangement, minus the Graveyard
and Your House, which are set aside. Every location that can hide a card has an empty
slot beside it for the Ephemeral Shop, which is put into play "adjacent to your
location" (see 'esotericaSlot'). Rivertown needs none: it never has a card beneath it.
-}
theDoomOfArkhamPartI :: Difficulty -> TheDoomOfArkhamPartI
theDoomOfArkhamPartI difficulty =
  scenario
    TheDoomOfArkhamPartI
    "11682"
    "The Doom of Arkham Pt I"
    difficulty
    [ ".                                        tillinghastEsotericaNorthside tillinghastEsotericaDowntown tillinghastEsotericaEasttown"
    , ".                                        northside                    downtown                     easttown"
    , "tillinghastEsotericaMiskatonicUniversity miskatonicUniversity         rivertown                    ."
    , "tillinghastEsotericaStMarysHospital      stMarysHospital              southside                    tillinghastEsotericaSouthside"
    ]
{- FOURMOLU_ENABLE -}

instance HasChaosTokenValue TheDoomOfArkhamPartI where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfArkhamPartI attrs) = case chaosTokenFace of
    Skull -> do
      -- "-X. X is half the number of locations with no scenario cards beneath them
      -- (rounded up) [-X. X is the number of locations with no scenario cards
      -- beneath them]."
      n <- selectCount $ LocationWithCardsUnderneath NoCards
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs ((n + 1) `div` 2) n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDoomOfArkhamPartI where
  runMessage msg s@(TheDoomOfArkhamPartI attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor do
        setTitle "title"
        p "intro1"
        p "intro2"
        p "intro3"
        p "intro4"
        p "intro5"
        p "intro6"
        p.basic.right "proceedToSetup"
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDoomOfArkhamPartI attrs do
      setup $ ul do
        li.nested "gatherSets" $ li "midnightMasksCards"
        li.nested "placeLocations" do
          li "randomVersions"
          li "setAsideLocations"
          li "startAtRivertown"
        li "cardsBeneathLocations"
        li "starSpawn"
        li "setCardsAside"
        li.nested "legrasse" $ li "legrasseInPlay"
        li "doomOnAgenda"
        li "floodTokens"
        li "buildEncounterDeck"
        unscoped $ li "readyToBegin"

      additionalRules "partsOfTheDoomOfArkham"

      gather Set.TheDoomOfArkhamPartI
      gather Set.DeepOnes
      gather Set.Domination
      gather Set.Dreams
      gather Set.StarSpawn
      gather Set.AgentsOfCthulhu
      -- "When gathering The Midnight Masks encounter set, only gather the location
      -- and treachery cards." Acts and agendas carry no encounter-set quantity, so
      -- they are never gathered; the set has nothing else in it.
      gather Set.TheMidnightMasks

      setActDeck [Acts.thePhantomShop]

      -- "Place doom on the agenda equal to the number of investigators." Preloaded
      -- so The Coming Storm enters play already holding it: placing it afterwards
      -- would trip the agenda's own "when doom is placed on this agenda" forced
      -- ability and flood the lead investigator's location during setup.
      playerCount <- getPlayerCount
      scenarioSetupModifier
        attrs.id
        attrs
        (AgendaId $ toCardCode Agendas.theComingStorm)
        (EntersPlayWithDoom playerCount)
      setAgendaDeck [Agendas.theComingStorm]

      -- Randall is spawned by the Ephemeral Shop, and the artifacts are stacked
      -- beneath it, so all of them wait out of play until it is drawn. "Gather all
      -- earned artifacts along with the Horror in Clay story asset": the sculpture
      -- is not one of the earned ones — it is what this scenario is played for.
      earnedArtifacts <- getEarnedArtifacts
      setAside $ Enemies.randallTillinghast : earnedArtifacts
      removeEvery [Assets.horrorInClay]
      setAsideCards [Assets.horrorInClay]

      -- Consume the gathered Midnight Masks locations; the placements below create
      -- exactly the copies this game uses. This is also what "remove the other
      -- versions of those locations from the game" amounts to: whichever Downtown
      -- and Southside lose the coin flip are simply never placed.
      removeCards =<< amongGathered (CardFromEncounterSet Set.TheMidnightMasks <> #location)

      -- "Set each other location aside, out of play."
      setAside [Locations.graveyard, Locations.yourHouse]

      downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      southside <- placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      northside <- place Locations.northside
      easttown <- place Locations.easttown
      stMarysHospital <- place Locations.stMarysHospital
      miskatonicUniversity <- place Locations.miskatonicUniversity
      startAt =<< place Locations.rivertown

      -- "Shuffle the single-sided Tillinghast Esoterica location with each treachery
      -- from The Midnight Masks encounter set and place one of them under each
      -- location except for Rivertown." Five treacheries plus the shop is exactly
      -- one card for each of the six.
      esoterica <- fromGathered1 Locations.tillinghastEsotericaEphemeralShop
      treacheries <- fromGathered (CardFromEncounterSet Set.TheMidnightMasks <> #treachery)
      hidden <- shuffleM (esoterica : treacheries)
      let hiding = [northside, downtown, easttown, miskatonicUniversity, stMarysHospital, southside]
      for_ (zip hiding hidden) \(lid, card) -> placeUnderneath lid [card]

      -- "Shuffle each enemy from the Star Spawn encounter set and remove two at
      -- random from the game. (Remove three instead if playing on easy mode.)"
      starSpawn <- shuffleM =<< amongGathered (CardFromEncounterSet Set.StarSpawn <> #enemy)
      removeCards $ take (if attrs.difficulty == Easy then 3 else 2) starSpawn

      -- "Choose an investigator to add the John Raymond Legrasse story asset to
      -- their deck... Put [it] into play under that investigator's control." He is
      -- created in play directly: AddCampaignCardToDeck only records a story card
      -- for the *next* scenario's deck, and this one's decks are already built, so
      -- routing him through the deck would leave nothing for setup to find.
      lead <- getLead
      investigators <- select Anyone
      legrasse <- genPlayerCard Assets.johnRaymondLegrasse
      chooseOrRunOneM lead do
        questionLabeled' "chooseLegrasseInvestigator"
        targets investigators \iid ->
          createAssetAt_ (PlayerCard $ setPlayerCardOwner iid legrasse) (InPlayArea iid)
    -- {cultist}: "If this test fails, place 1 doom on the nearest enemy with no doom
    -- on it." {tablet}: "If this test fails, place 1 of your clues on your location."
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> do
          nearest <- select $ NearestEnemyTo iid (EnemyWithDoom $ static 0)
          unless (null nearest) $ chooseOrRunOneM iid $ targets nearest \eid ->
            placeDoom Cultist eid 1
        Tablet -> placeCluesOnLocation iid Tablet 1
        _ -> pure ()
      pure s
    -- {elderThing}: "If your location is flooded, reveal another token."
    ResolveChaosToken _ ElderThing iid -> do
      whenAny (locationWithInvestigator iid <> FloodedLocation) $ drawAnotherChaosToken iid
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        Resolution 1 -> resolution1 attrs
        -- The Phantom Shop's "otherwise" branch — fewer than 5 Artifact assets —
        -- comes straight here, past the Campaign Log update in Resolution 1.
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          endOfScenarioThen TheDoomOfArkhamPartII
        NoResolution -> do
          resolution "noResolution"
          -- "Each investigator searches the collection for a random basic Injury or
          -- Madness weakness and adds it to their deck."
          eachInvestigator \iid ->
            searchCollectionForRandomBasicWeakness iid attrs [Injury, Madness]
          resolution1 attrs
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> TheDoomOfArkhamPartI <$> liftRunMessage msg attrs

{- | "Proceed to Resolution 1." Reached both by The Phantom Shop's 5-artifact branch
and by the defeat/resign ending, so the Campaign Log update, the experience, and the
choice of when to play Part II all live here.
-}
resolution1 :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m ()
resolution1 attrs = do
  -- "Cross out the name of each Artifact asset that was not under any investigator's
  -- control when the scenario ended", and the Horror in Clay is earned the other way
  -- around: it is only checked off if an investigator held it at the end.
  for_ artifactAssets \(key, def) -> do
    held <- selectAny $ artifactInPlay def <> AssetControlledBy Anyone
    if key == HorrorInClay
      then when held $ record HorrorInClay
      else whenM (getHasRecord key) $ unless held $ crossOut key
  -- "In your Campaign Log, record 'Flooded Neighborhoods:' along with a list of each
  -- flooded location."
  flooded <- traverse (fieldMap LocationCard toCardCode) =<< select FloodedLocation
  unless (null flooded) $ recordSetInsert FloodedNeighborhoods flooded

  resolutionWithXpAndChooseOne "resolution1" (allGainXp' attrs) do
    labeled' "readyToFight" $ resolution "resolution2" >> endOfScenarioThen TheDoomOfArkhamPartII
    labeled' "needMoreTime" $ resolution "resolution3" >> endOfScenarioThen TheDoomOfArkhamPartII
