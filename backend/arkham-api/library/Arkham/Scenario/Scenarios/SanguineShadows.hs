module Arkham.Scenario.Scenarios.SanguineShadows (sanguineShadows) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationConcealedCards))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.SanguineShadows.Helpers
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Central, Coterie, Criminal))

newtype SanguineShadows = SanguineShadows ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor SanguineShadows where
  getModifiersFor (SanguineShadows attrs) = do
    modifySelectWith
      attrs
      (not_ $ LocationWithToken Token.Target)
      setActiveDuringSetup
      [CampaignModifier "noConcealed[LaChicaRoja]"]
    modifySelectWith
      attrs
      (oneOf [LocationWithToken Token.Target, LocationWithTrait Central])
      setActiveDuringSetup
      [CampaignModifier "noConcealed[ApportionedKa]"]

sanguineShadows :: Difficulty -> SanguineShadows
sanguineShadows difficulty =
  scenario
    SanguineShadows
    "09545"
    "Sanguine Shadows"
    difficulty
    [ ". . b b c c . ."
    , ". . b b c c . ."
    , ". . b b c c . ."
    , "h h . . . . d d"
    , "h h . a a . d d"
    , "h h . a a . d d"
    , "g g . a a . e e"
    , "g g . . . . e e"
    , "g g . f f . e e"
    , ". . . f f . . ."
    , ". . . f f . . ."
    ]

instance HasChaosTokenValue SanguineShadows where
  getChaosTokenValue iid tokenFace (SanguineShadows attrs) = case tokenFace of
    Skull -> do
      let x = attrs.token Token.Target
      pure $ toChaosTokenValue attrs Skull x (x + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 5 7
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage SanguineShadows where
  runMessage msg s@(SanguineShadows attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "everything" $ doStep 2 PreScenarioSetup
        labeled' "breadcrumbs" $ doStep 3 PreScenarioSetup
        labeled' "insist" $ doStep 4 PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      criminal <- selectAny $ InvestigatorWithTrait Criminal
      flavor $ setTitle "title" >> p "intro2Part1" >> p.validate criminal "criminal" >> p "intro2Part2"
      remember MatiasBolivarTrustsYou
      swapTokens ElderThing Tablet
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      remember MatiasBolivarDoesntTrustYou
      swapTokens Tablet ElderThing
      pure s
    Setup -> runScenarioSetup SanguineShadows attrs do
      setup $ ul do
        li "gatherSets"
        li "decks"
        li.nested "placeStart" do
          li "beginPlay"
        li "placeRemaining"
        li.nested "placeTargets" do
          li "targets"
        li "miniCards"
        li "laChicaRoja"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"
      gather Set.SanguineShadows
      gather Set.DarkVeiling
      gather Set.MysteriesAbound
      gather Set.ShadowOfADoubt
      gather Set.StrangeHappenings
      gather Set.LockedDoors
      gather Set.Nightgaunts
      handleRedCoterie

      setAgendaDeck [Agendas.whereIsShe]
      setActDeck [Acts.theScarletShadow, Acts.inTheSearchlight]

      setAside
        [ Enemies.theSanguineWatcherWithTheRubySpectacles
        , Enemies.apportionedKa
        , Agendas.seeingRed
        , Keys.theWeepingLady
        ]

      startAt =<< place Locations.avenidaDeMayo
      otherLocations <-
        placeAllCapture
          . drop 1
          =<< shuffle
            [ Locations.casaRosada
            , Locations.catedralMetropolitana
            , Locations.cementarioDeLaRecoleta
            , Locations.palacioErrazuriz
            , Locations.theCabildo
            , Locations.bancoDeLaProvincia
            , Locations.teatroColon
            ]

      for_ otherLocations $ placeTokensOn ScenarioSource Token.Target 1

      lead <- getLead
      laChicaRoja <- fetchCard Enemies.laChicaRojaTheGirlInTheCarmineCoat
      drawCard lead laChicaRoja
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | n >= 2 -> do
          locations <-
            selectWithField LocationConcealedCards
              $ orConnected NotForMovement (locationWithInvestigator iid)
              <> LocationWithConcealedCard
          let (ls, cs) = unzip $ concatMap (\(l, cs') -> (l,) <$> cs') locations
          cs' <- shuffleM cs
          for_ (zip ls cs') \(l, c) -> push $ PlaceConcealedCard iid c (AtLocation l)
        ElderThing -> do
          coteries <- select $ NearestEnemyToFallback iid (EnemyWithTrait Coterie)
          chooseOneM iid do
            withI18n $ countVar 1 $ labeled' "placeAgendaDoom" $ placeDoomOnAgenda 1
            scenarioI18n $ labeledValidate' (notNull coteries) "coterieAttack" do
              chooseTargetM iid coteries \targetCoterie -> initiateEnemyAttack targetCoterie ElderThing iid
        _ -> pure ()
      pure s
    ResolveChaosToken drawnToken Tablet _iid -> do
      withSkillTest \sid -> do
        skillTestModifier sid drawnToken.face sid CancelSkills
        push CancelSkillEffects
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          getCurrentAgendaStep >>= \case
            3 -> do
              record TheSanguineWatchersTormentContinues
              record LaChicaRojaIsOnYourSide
              setBearer Keys.theWeepingLady $ keyWithEnemy Enemies.theSanguineWatcherHeSeesWhatIsNotThere
              markTime 2
              resolutionWithXp "noResolutionSeeingRed" $ allGainXpWithBonus' attrs $ toBonus "bonus.insight" 3
              endOfScenario
            _ -> do
              resolution "noResolution"
              push R3
        Resolution 1 -> do
          n <- countScenarioTokens Token.Target
          record YouHaventSeenTheLastOfLaChicaRoja
          markTime 1
          resolutionWithXp "resolution1"
            $ if n > 0
              then allGainXpWithBonus' attrs $ toBonus "bonus.targets" n
              else allGainXp' attrs
          chooseBearer Keys.theWeepingLady
          endOfScenario
        Resolution 2 -> do
          record YouHaventSeenTheLastOfTheSanguineWatcher
          record LaChicaRojaIsOnYourSide
          markTime 3
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus.insight" 3
          chooseBearer Keys.theWeepingLady
          endOfScenario
        Resolution 3 -> do
          n <- countScenarioTokens Token.Target
          record YouHaventSeenTheLastOfLaChicaRoja
          setBearer Keys.theWeepingLady $ keyWithEnemy Enemies.laChicaRojaHotOnYourTrail
          markTime 1
          resolutionWithXp "resolution3"
            $ if n > 0
              then allGainXpWithBonus' attrs $ toBonus "bonus.targets" n
              else allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> SanguineShadows <$> liftRunMessage msg attrs
