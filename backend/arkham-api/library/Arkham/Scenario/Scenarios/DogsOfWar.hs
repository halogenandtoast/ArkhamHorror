module Arkham.Scenario.Scenarios.DogsOfWar (dogsOfWar) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (patrol)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, isFightWith)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (assetAt)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DogsOfWar.Helpers
import Arkham.Trait (Trait (LocusSite, Miskatonic, Scholar))

newtype DogsOfWar = DogsOfWar ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dogsOfWar :: Difficulty -> DogsOfWar
dogsOfWar difficulty =
  scenario
    DogsOfWar
    "09635"
    "Dogs of War"
    difficulty
    [ ".      triangle .    equals ."
    , "square triangle star equals squiggle"
    , "square circle   star moon   squiggle"
    , ".      circle   .    moon   ."
    ]

data Version = Version1 | Version2 | Version3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasChaosTokenValue DogsOfWar where
  getChaosTokenValue iid tokenFace (DogsOfWar attrs) = case tokenFace of
    Skull -> do
      atLocus <- runValidT $ MaybeT (getLocationOf iid) >>= liftGuardM . (`matches` LocationWithKeyLocus)
      pure $ toChaosTokenValue attrs Skull (if atLocus then 3 else 1) (if atLocus then 4 else 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    ElderThing -> pure $ toChaosTokenValue attrs Cultist 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DogsOfWar where
  runMessage msg s@(DogsOfWar attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      t <- getTime
      flavor do
        setTitle "title"
        p "intro1"
        ul do
          li.validate (t < 20) "fewerThanTwentyTime"
          li.validate (t >= 20) "twentyOrMoreTime"
      doStep (if t < 20 then 2 else 3) PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      miskatonicOrScholar <- selectAny $ mapOneOf InvestigatorWithTrait [Miskatonic, Scholar]
      storyWithChooseOneM'
        do
          setTitle "title"
          p "intro2Part1"
          p.validate miskatonicOrScholar "miskatonicOrScholar"
          p "intro2Part2"
        do
          labeled' "weAccept" $ doStep 4 PreScenarioSetup
          labeled' "refuse" $ doStep 5 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro3") do
        labeled' "weAccept" $ doStep 6 PreScenarioSetup
        labeled' "refuse" $ doStep 7 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version1
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro5"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version2
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro6"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version3
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro7"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version3
    Setup -> do
      case getMetaKeyDefault "version" Version1 attrs of
        Version1 -> doStep 1 Setup
        Version2 -> doStep 2 Setup
        Version3 -> doStep 3 Setup
      pure s
    DoStep 1 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version1" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li.nested "keyLocus" do
            li "doom"
          li "otherLocations"
          li "theClaretKnight"
          li "theBeast"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.DarkCult
      handleRedCoterie

      setAgendaDeck [Agendas.brewingCatastropheV1]
      setActDeck [Acts.rabbitsWhoRunV1]
      startAt =<< place Locations.qaitbayCitadel
      locusSites <-
        placeAllCapture
          [ Locations.windsorPalaceHotel
          , Locations.victoriaCollege
          , Locations.theCorniche
          , Locations.zanEtElSettat
          ]
      for_ locusSites \locusSite -> do
        assetAt_ Assets.keyLocusLastBastion locusSite
        placeDoom attrs locusSite 1
      theBourse <- place Locations.theBourseLocusSafeguard
      catacombs <- place Locations.catacombsOfKomElShoqafaBloodyNexus
      removeEvery
        [ Locations.theBourseCoteriePost
        , Locations.theBourseCommercialCenter
        , Locations.catacombsOfKomElShoqafaAncientTomb
        , Locations.catacombsOfKomElShoqafaDenOfTheBeast
        ]
      theClaretKnight <- assetAt Assets.theClaretKnightHerSwornChampion theBourse
      createScarletKeyAt_ Keys.theLightOfPharos $ AttachedToAsset theClaretKnight Nothing
      enemyAt_ Enemies.theBeastInACowlOfCrimsonRavagerInRed catacombs
    DoStep 2 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version2" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li "keyLocus"
          li "otherLocations"
          li "theClaretKnight"
          li "theBeast"
          li "miniCards"
          li "coterieAssassin"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.CleanupCrew
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      handleRedCoterie

      setAgendaDeck [Agendas.brewingCatastropheV2]
      setActDeck [Acts.rabbitsWhoRunV2]
      startAt =<< place Locations.qaitbayCitadel
      locusSites <-
        placeAllCapture
          [ Locations.windsorPalaceHotel
          , Locations.victoriaCollege
          , Locations.theCorniche
          , Locations.zanEtElSettat
          ]
      for_ locusSites $ assetAt_ Assets.keyLocusDefensiveBarrier
      theBourse <- place Locations.theBourseCoteriePost
      place_ Locations.catacombsOfKomElShoqafaAncientTomb
      removeEvery
        [ Locations.theBourseLocusSafeguard
        , Locations.theBourseCommercialCenter
        , Locations.catacombsOfKomElShoqafaBloodyNexus
        , Locations.catacombsOfKomElShoqafaDenOfTheBeast
        , Enemies.theBeastInACowlOfCrimsonRavagerInRed
        ]
      theClaretKnight <- createEnemyAt Enemies.theClaretKnightCoterieKingpin theBourse
      createScarletKeyAt_ Keys.theLightOfPharos $ AttachedToEnemy theClaretKnight
      lead <- getLead
      drawCard lead =<< fetchCard Enemies.coterieAssassinA
    DoStep 3 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version3" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li "keyLocus"
          li "otherLocations"
          li "theBeast"
          li "theClaretKnight"
          li "miniCards"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.CleanupCrew
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.DarkCult
      setAgendaDeck [Agendas.brewingCatastropheV3]
      setActDeck [Acts.rabbitsWhoRunV3]

      startAt =<< place Locations.qaitbayCitadel
      locusSites <-
        placeAllCapture
          [ Locations.windsorPalaceHotel
          , Locations.victoriaCollege
          , Locations.theCorniche
          , Locations.zanEtElSettat
          ]
      for_ locusSites $ assetAt_ Assets.keyLocusDefensiveBarrier
      place_ Locations.theBourseCommercialCenter
      catacombs <- place Locations.catacombsOfKomElShoqafaDenOfTheBeast
      removeEvery
        [ Locations.theBourseLocusSafeguard
        , Locations.theBourseCoteriePost
        , Locations.catacombsOfKomElShoqafaBloodyNexus
        , Locations.catacombsOfKomElShoqafaAncientTomb
        , Enemies.theClaretKnightCoterieKingpin
        ]
      theBeast <- createEnemyAt Enemies.theBeastInACowlOfCrimsonWolfInSheepsClothing catacombs
      createScarletKeyAt_ Keys.theLightOfPharos $ AttachedToEnemy theBeast
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> do
          ls <- select $ NearestLocationTo iid $ oneOf [LocationWithTrait LocusSite, LocationWithKeyLocus]
          ts <-
            keyLocusTargets
              >>= filterM (`matches` TargetAtLocation (mapOneOf LocationWithId ls))
          chooseOneM iid $ withI18n do
            numberVar "damage" 1
              $ numberVar "horror" 1
              $ labeled' "takeDamageAndHorror"
              $ assignDamageAndHorror iid Cultist 1 1
            targets ls $ placeDoomOn Cultist 1
            targets ts $ placeDoomOn Cultist 1
        ElderThing -> do
          let x = if isEasyStandard attrs then min 3 n else n
          loseResources iid ElderThing x
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenM (isFightWith PatrolEnemy) do
        whenJustM getSkillTestTargetedEnemy \enemy -> do
          afterSkillTestQuiet do
            when (isHardExpert attrs) $ initiateEnemyAttack enemy Cultist iid
            disengageFromAll enemy
            patrol enemy
      pure s
    _ -> DogsOfWar <$> liftRunMessage msg attrs
