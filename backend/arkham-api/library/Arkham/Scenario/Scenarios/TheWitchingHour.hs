module Arkham.Scenario.Scenarios.TheWitchingHour (setupTheWitchingHour, theWitchingHour, TheWitchingHour (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActStep (..), actStep)
import Arkham.Act.Types (Field (ActSequence))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.ChaosBag
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Scenario qualified as Scenario
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Xp
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheWitchingHour.Helpers
import Arkham.Tarot
import Arkham.Trait (Trait (Witch))
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map

newtype TheWitchingHour = TheWitchingHour ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchingHour :: Difficulty -> TheWitchingHour
theWitchingHour difficulty =
  scenario
    TheWitchingHour
    "05050"
    "The Witching Hour"
    difficulty
    [ ".      .     woods1        .      . "
    , ".      .     .             .      . "
    , "woods2 .     witchesCircle .      woods3"
    , ".      .     .             .      ."
    , ".      wood4 .             woods5 ."
    ] -- lost and separated, do we label 4 zones, or do a different placement

instance HasChaosTokenValue TheWitchingHour where
  getChaosTokenValue iid chaosTokenFace (TheWitchingHour attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    Tablet -> pure $ toChaosTokenValue attrs Skull 1 2
    ElderThing -> pure $ toChaosTokenValue attrs Skull 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

setupTheWitchingHour
  :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheWitchingHour attrs = do
  setup $ ul do
    li "gatherSets"
    li "gatherAgentSets"
    li.nested "placeLocations" do
      li "placeWitchHauntedWoods"
      li "removeRemainingWitchHauntedWoods"
      li "startAtWitchHauntedWoods"
    li "setAside"
    unscoped $ li "shuffleRemainder"
  -- The Devourer Below is only locations
  whenReturnTo $ gather Set.ReturnToTheWitchingHour
  gather Set.TheWitchingHour
  gather Set.AnettesCoven
  gather Set.CityOfSins `orWhenReturnTo` gather Set.CityOfTheDamned
  gather Set.Witchcraft `orWhenReturnTo` gather Set.Hexcraft
  gather Set.AncientEvils `orWhenReturnTo` gather Set.ImpendingEvils
  gather Set.StrikingFear

  gatherAndSetAside Set.AgentsOfShubNiggurath
  gatherAndSetAside Set.AgentsOfAzathoth

  isReturnTo <- getIsReturnTo

  witchHauntedWoods <-
    sampleN 5
      $ Locations.witchHauntedWoodsAbandonedMine
      :| ( [ Locations.witchHauntedWoodsCairnStones
           , Locations.witchHauntedWoodsTheLonelyTree
           , Locations.witchHauntedWoodsChildsTreeHouse
           , Locations.witchHauntedWoodsTaintedWell
           , Locations.witchHauntedWoodsHermitsHouse
           , Locations.witchHauntedWoodsOvergrownBarn
           ]
             <> ( guard isReturnTo
                    *> [Locations.witchHauntedWoodsWitchTree, Locations.witchHauntedWoodsUnmarkedGraveyard]
                )
         )

  setAside
    $ [ Enemies.anetteMason
      , Locations.arkhamWoodsUnhallowedGround
      , Locations.arkhamWoodsTwistingPaths
      , Locations.arkhamWoodsOldHouse
      , Locations.arkhamWoodsCliffside
      , Locations.arkhamWoodsTangledThicket
      ]
    <> [Locations.arkhamWoodsQuietGlade | not isReturnTo]
    <> ( guard isReturnTo
           *> [ Locations.arkhamWoodsHiddenPath
              , Locations.arkhamWoodsPlaceOfPower
              , Locations.arkhamWoodsBootleggingOperation
              ]
       )

  whenReturnTo do
    returnToArkhamWoods <-
      genCards
        [ Locations.arkhamWoodsGreatWillow
        , Locations.arkhamWoodsLakeside
        , Locations.arkhamWoodsCorpseRiddenClearing
        , Locations.arkhamWoodsWoodenBridge
        ]

    leadChooseOneM do
      questionLabeled' "gatherTheDevourerBelow"
      questionLabeledCard (CardCode "54017b")
      unscoped $ labeled' "yes" $ push $ SetAsideCards returnToArkhamWoods
      unscoped $ labeled' "no" nothing
    removeOneOf Locations.arkhamWoodsQuietGlade

  iids <- getInvestigators
  let
    woodsWithInvestigators = zip (cycleN 5 iids) witchHauntedWoods
    locationMap =
      foldMap
        (\(investigator, location) -> MonoidalMap.singleton investigator (location :| []))
        woodsWithInvestigators
  startingLocations <-
    Map.fromList <$> traverse (\(k, v) -> (k,) <$> sample v) (MonoidalMap.toList locationMap)

  for_ woodsWithInvestigators \(investigator, location) -> do
    lid <- place location
    push $ PutLocationInFrontOf investigator lid
    runMaybeT do
      startingLocation <- hoistMaybe $ lookup investigator startingLocations
      guard $ location == startingLocation
      lift $ moveTo_ attrs investigator lid

  setAgendaDeck [Agendas.temperanceXIV, Agendas.theNightHowls]
  setActDeck
    $ [Acts.lostInTheWoods, Acts.witchHauntings, Acts.pathsIntoTwilight, Acts.aCircleUnbroken]
    <> (guard isReturnTo *> [Acts.returnToACircleUnbroken])

instance RunMessage TheWitchingHour where
  runMessage msg s@(TheWitchingHour attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "avoidFate" $ doStep 2 PreScenarioSetup
        labeled' "bullshit" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      record YouHaveAcceptedYourFate
      addChaosToken Tablet
      addChaosToken Tablet

      let
        takeCards = do
          -- collection is infinite so we only care if the lead already has either card in their deck
          lead <- getLead
          addCards <-
            fieldMap
              InvestigatorDeck
              (not . any ((`elem` [Assets.theTowerXVI, Assets.aceOfRods1]) . toCardDef))
              lead
          when addCards do
            addCampaignCardToDeck lead ShuffleIn Assets.theTowerXVI
            addCampaignCardToDeck lead ShuffleIn Assets.aceOfRods1

      isReturnTo <- Scenario.getIsReturnTo
      if isReturnTo
        then do
          leadPlayer <- getLeadPlayer
          storyWithChooseOneM' (setTitle "title" >> p "choose") do
            labeled' "performTarotReading" do
              push
                $ Ask leadPlayer
                $ PickDestiny
                $ zipWith
                  DestinyDrawing
                  [ "theWitchingHour"
                  , "atDeathsDoorStep"
                  , "theSecretName"
                  , "theWagesOfSin"
                  , "forTheGreaterGood"
                  , "unionAndDisillusion"
                  , "inTheClutchesOfChaos"
                  , "beforeTheBlackThrone"
                  ]
                $ map
                  (TarotCard Upright)
                  [ TemperanceXIV
                  , JusticeXI
                  , TheHermitIX
                  , TheHangedManXII
                  , TheHierophantV
                  , TheLoversVI
                  , TheChariotVII
                  , WheelOfFortuneX
                  ]
            labeled' "takeCards" takeCards
        else takeCards

      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      record YouHaveRejectedYourFate
      addChaosToken ElderThing
      addChaosToken ElderThing
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      pure s
    Setup -> runScenarioSetup TheWitchingHour attrs $ setupTheWitchingHour attrs
    ScenarioResolution r -> scope "resolutions" do
      step <- actStep <$> selectJustField ActSequence AnyAct
      case r of
        NoResolution -> do
          resolution "noResolution"
          push $ if step == ActStep 4 then R4 else R3
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          record TheWitches'SpellWasBroken
          recordSetInsert MementosDiscovered [MesmerizingFlute, RitualComponents]
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          record TheWitches'SpellWasBroken
          recordSetInsert MementosDiscovered [MesmerizingFlute, ScrapOfTornShadow]
          endOfScenario
        Resolution 3 -> do
          resolutionWithXp "resolution3"
            $ if step == ActStep 3
              then allGainXpWithBonus' attrs $ toBonus "bonus" 1
              else allGainXp' attrs
          record TheWitches'SpellWasCast
          when (step == ActStep 3) $ recordSetInsert MementosDiscovered [MesmerizingFlute]
          endOfScenario
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXpWithBonus' attrs $ toBonus "bonus" 1
          record TheWitches'SpellWasCast
          recordSetInsert MementosDiscovered [MesmerizingFlute]
          endOfScenario
        Resolution 5 -> do
          resolutionWithXp "resolution5" $ allGainXpWithBonus' attrs $ toBonus "bonus" 3
          record TheWitches'SpellWasCast
          record ErynnWantsToMeet
          recordSetInsert MementosDiscovered [MesmerizingFlute, StrangeIncantation]
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    ResolveChaosToken _ Skull iid | isHardExpert attrs -> do
      getSkillTestDifficulty >>= traverse_ (discardTopOfEncounterDeck iid Skull)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Skull | isEasyStandard attrs -> discardTopOfEncounterDeck iid Skull n
        Tablet -> afterSkillTestQuiet $ forTarget attrs msg
        ElderThing -> do
          enemies <-
            select
              $ enemy_
              $ withTrait Witch
              <> #exhausted
              <> at_ (orConnected NotForMovement $ locationWithInvestigator iid)
          if isEasyStandard attrs
            then chooseTargetM iid enemies \enemy -> do
              readyThis enemy
              healAllDamage ElderThing enemy
            else for_ enemies \enemy -> do
              readyThis enemy
              healAllDamage ElderThing enemy
        _ -> pure ()
      pure s
    ForTarget (isTarget attrs -> True) (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) -> do
      when (token.face == Tablet) do
        bottomTreachery <-
          take 1 . reverse . filterCards (card_ #treachery) <$> getEncounterDiscard RegularEncounterDeck
        for_ bottomTreachery (drawCard iid)
      pure s
    _ -> TheWitchingHour <$> liftRunMessage msg attrs
