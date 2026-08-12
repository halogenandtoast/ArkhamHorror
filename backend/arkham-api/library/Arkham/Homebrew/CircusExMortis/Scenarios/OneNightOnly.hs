module Arkham.Homebrew.CircusExMortis.Scenarios.OneNightOnly (oneNightOnly) where

import Arkham.Act.Types (Field (..))
import Arkham.Card (toCardDef)
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveTo_, moveTowardsMatching)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Creature, Performer))

newtype OneNightOnly = OneNightOnly ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneNightOnly :: Difficulty -> OneNightOnly
oneNightOnly difficulty =
  scenario
    OneNightOnly
    ":circus-ex-mortis:001"
    "One Night Only"
    difficulty
    [ ". carousel carousel . . gamesGallery gamesGallery ."
    , ". . . theBigTopFirstRing theBigTopFirstRing . . ."
    , "animalCages animalCages theBigTopSecondRing theBigTopSecondRing theBigTopThirdRing theBigTopThirdRing performerTrailers performerTrailers"
    ]

behindTheCurtainMatcher :: InvestigatorMatcher
behindTheCurtainMatcher =
  oneOf [investigatorIs Investigators.dexterDrake, InvestigatorWithTrait Performer]

instance HasChaosTokenValue OneNightOnly where
  getChaosTokenValue iid tokenFace (OneNightOnly attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ EnemyWithTrait Creature
      pure $ ChaosTokenValue Skull (NegativeModifier $ if isHardExpert attrs then n + 1 else n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    ElderThing -> do
      vsCreature <-
        getSkillTestTarget >>= \case
          Just (EnemyTarget eid) -> eid <=~> EnemyWithTrait Creature
          _ -> pure False
      pure
        $ if vsCreature
          then toChaosTokenValue attrs ElderThing 3 4
          else toChaosTokenValue attrs ElderThing 1 2
    MoonToken -> pure moonTokenValue
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage OneNightOnly where
  runMessage msg s@(OneNightOnly attrs) = runQueueT $ scenarioI18n "oneNightOnly" $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      behindTheCurtain <- select behindTheCurtainMatcher
      storyOnlyBuild behindTheCurtain $ setTitle "title" >> p "behindTheCurtain"
      pure s
    Setup -> runScenarioSetup OneNightOnly attrs do
      gather Set.OneNightOnly
      gather Set.IllusoryTricks
      gather Set.NewMoonDaredevils
      gather Set.NewMoonEntertainers
      gather Set.PanickedMasses
      gather Set.PrimordialEvils

      setAside
        [ Assets.illusoryLocus
        , Locations.circusGatesPathToFreedom
        , Enemies.disguisedMonstrosity
        ]

      firstRing <- place Locations.theBigTopFirstRing
      secondRing <- place Locations.theBigTopSecondRing
      thirdRing <- place Locations.theBigTopThirdRing
      placeAll
        [ Locations.carousel
        , Locations.gamesGallery
        , Locations.animalCages
        , Locations.performerTrailers
        ]

      exhaustThis =<< enemyAt Enemies.disguisedMonstrosity firstRing

      actOne <- sample (fmap fst ratsInACageVariants)
      setActDeck [actOne, Acts.smokeAndMirrors, Acts.outAndAway]
      setAgendaDeck [Agendas.theTrueFace, Agendas.houseOfHorrors, Agendas.mesmericMagic]

      eachInvestigator \iid -> do
        chooseOneM iid $ scope "seats" do
          labeled' "firstRing" $ moveTo_ attrs iid firstRing
          labeled' "secondRing" $ moveTo_ attrs iid secondRing
          labeled' "thirdRing" $ moveTo_ attrs iid thirdRing
        behind <- iid <=~> behindTheCurtainMatcher
        when behind $ gainClues iid attrs 1
    FailedSkillTestWithToken iid Cultist -> do
      moveTowardsMatching attrs iid (NearestLocationToYou $ LocationWithTitle "The Big Top")
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "resolution1"
          record TheRingmasterDoesNotSuspectYou
          actStep <- getCurrentActStep
          when (actStep == 1) do
            selectOne AnyAct >>= traverse_ \aid -> do
              def <- fieldMap ActCard toCardDef aid
              for_ (lookupRatsInACage def) (addChaosToken . snd)
          push R3
        Resolution 2 -> do
          resolution "resolution2"
          record TheRingmasterHasHisEyeOnYou
          push R3
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> OneNightOnly <$> liftRunMessage msg attrs
