module Arkham.Homebrew.CircusExMortis.Scenarios.PiperAtTheGatesOfDawn (piperAtTheGatesOfDawn) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Helpers.Xp (XpBonus (NoBonus), toBonus)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Elite, Hex, Performer))

newtype PiperAtTheGatesOfDawn = PiperAtTheGatesOfDawn ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

piperAtTheGatesOfDawn :: Difficulty -> PiperAtTheGatesOfDawn
piperAtTheGatesOfDawn difficulty =
  scenario
    PiperAtTheGatesOfDawn
    ":circus-ex-mortis:108"
    "Piper at the Gates of Dawn"
    difficulty
    [ ". . . circusGates circusGates . . ."
    , ". carousel carousel . . gamesGallery gamesGallery ."
    , ". . . theBigTopFirstRing theBigTopFirstRing . . ."
    , ". . . sylvesterBlake sylvesterBlake . . ."
    , "animalCages animalCages theBigTopSecondRing theBigTopSecondRing theBigTopThirdRing theBigTopThirdRing performerTrailers performerTrailers"
    ]

{- | Which "Audience Participation" the scenario uses, keyed to the days the
ringmaster had to prepare (recorded by All Points West). The same count is also
the number of resources that start on the scenario reference card.
-}
audienceParticipationFor :: Int -> CardDef
audienceParticipationFor days
  | days == 0 = Acts.audienceParticipationVI
  | days <= 4 = Acts.audienceParticipationVII
  | otherwise = Acts.audienceParticipationVIII

audienceParticipations :: [CardDef]
audienceParticipations =
  [Acts.audienceParticipationVI, Acts.audienceParticipationVII, Acts.audienceParticipationVIII]

instance HasChaosTokenValue PiperAtTheGatesOfDawn where
  getChaosTokenValue iid tokenFace (PiperAtTheGatesOfDawn attrs) = case tokenFace of
    Skull -> do
      resources <- countScenarioTokens Token.Resource
      pure
        $ if resources >= 5
          then toChaosTokenValue attrs Skull 4 5
          else toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> do
      vsElite <- runDefaultMaybeT False do
        eid <- MaybeT getSkillTestTargetedEnemy
        eid `matches` EnemyWithTrait Elite
      pure
        $ if vsElite
          then toChaosTokenValue attrs ElderThing 5 6
          else toChaosTokenValue attrs ElderThing 3 4
    MoonToken -> pure moonTokenValue
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PiperAtTheGatesOfDawn where
  runMessage msg s@(PiperAtTheGatesOfDawn attrs) = runQueueT $ scenarioI18n "piperAtTheGatesOfDawn" $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM
        do
          h "title"
          p "body1"
          p "body2"
          p "body3"
          ul $ li.nested "decide" do
            li "hitHard"
            li "slowAndSteady"
        do
          labeled "hitHard" $ whenM (selectAny $ ChaosTokenFaceIs Tablet) do
            removeChaosToken Tablet
            addChaosToken Cultist
          labeled "slowAndSteady" $ whenM (selectAny $ ChaosTokenFaceIs Cultist) do
            removeChaosToken Cultist
            addChaosToken Tablet
      pure s
    Setup -> runScenarioSetup PiperAtTheGatesOfDawn attrs do
      days <- getRecordCount TheRingmasterHadDaysToPrepare
      let actTwo = audienceParticipationFor days

      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li.nested "checkLog" do
          li.validate (days == 0) "noDays"
          li.validate (days >= 1 && days <= 4) "someDays"
          li.validate (days >= 5) "manyDays"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.PiperAtTheGatesOfDawn
      gather Set.CircusGrounds
      gather Set.ChildrenOfTheGoat
      gather Set.IllusoryTricks
      gather Set.LunaticNight
      gather Set.NewMoonDaredevils
      gather Set.NewMoonEntertainers

      placeAll
        [ Locations.theBigTopFirstRing
        , Locations.theBigTopSecondRing
        , Locations.theBigTopThirdRing
        , Locations.carousel
        , Locations.gamesGallery
        , Locations.animalCages
        , Locations.performerTrailers
        ]
      startAt =<< place Locations.circusGatesDoorwayToDoom

      when (days > 0) $ placeTokens attrs ScenarioTarget Token.Resource days
      removeEvery $ filter (/= actTwo) audienceParticipations

      setAside [Enemies.sylvesterBlake]

      setAgendaDeck [Agendas.repeatShowing, Agendas.doomAndGloom, Agendas.whirlingSpectacle]
      setActDeck [Acts.allsFair, actTwo, Acts.theTrueMonster]
    ResolveChaosToken _ Cultist iid -> do
      hexHere <- selectAny $ TreacheryAt (locationWithInvestigator iid) <> withTrait Hex
      when hexHere $ randomDiscardN iid attrs (if isEasyStandard attrs then 1 else 2)
      pure s
    ResolveChaosToken _ Tablet iid -> do
      performerHere <- selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Performer
      when performerHere $ loseResources iid attrs (if isEasyStandard attrs then 1 else 2)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        -- Every investigator was defeated or resigned; how far the acts got
        -- decides whether they died in the trap, escaped it, or unmasked Blake.
        NoResolution -> do
          resolution "noResolution"
          act <- getCurrentActStep
          push $ case act of
            1 -> R1
            2 -> R2
            _ -> R3
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsCouldNotEscapeTheCircus
          selectEach UneliminatedInvestigator $ push . InvestigatorKilled (toSource attrs)
          push GameOver
          endOfScenario
        Resolution 2 -> do
          resolution "resolution2"
          record TheInvestigatorsClashedWithBlake
          push R5
        Resolution 3 -> do
          resolution "resolution3"
          record TheInvestigatorsUnmaskedBlake
          push R5
        Resolution 4 -> do
          resolution "resolution4"
          record TheInvestigatorsStruckDownBlake
          push R5
        Resolution 5 -> do
          -- Resolution 3's bonus is carried here so both grants land in one report.
          unmasked <- getHasRecord TheInvestigatorsUnmaskedBlake
          wholeBigTopRevealed <- selectNone $ bigTopRings <> UnrevealedLocation
          addChaosToken ElderThing
          resolutionWithXp "resolution5"
            $ allGainXpWithBonus' attrs
            $ (if unmasked then toBonus "unmaskedBlake" 1 else NoBonus)
            <> (if wholeBigTopRevealed then toBonus "bigTop" 1 else NoBonus)
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> PiperAtTheGatesOfDawn <$> liftRunMessage msg attrs
