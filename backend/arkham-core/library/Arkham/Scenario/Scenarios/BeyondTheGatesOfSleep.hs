module Arkham.Scenario.Scenarios.BeyondTheGatesOfSleep (
  BeyondTheGatesOfSleep (..),
  beyondTheGatesOfSleep,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Card
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText
import Arkham.Trait (
  Trait (
    Assistant,
    Criminal,
    Drifter,
    Hunter,
    Illicit,
    Medic,
    Miskatonic,
    Relic,
    Scholar,
    Supply,
    Tactic,
    Tome,
    Veteran,
    Wayfarer,
    Weapon,
    Woods
  ),
 )
import Data.Aeson (Result (..))
import Data.List.NonEmpty qualified as NE

newtype BeyondTheGatesOfSleep = BeyondTheGatesOfSleep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheGatesOfSleep :: Difficulty -> BeyondTheGatesOfSleep
beyondTheGatesOfSleep difficulty =
  scenarioWith
    BeyondTheGatesOfSleep
    "06039"
    "Beyond the Gates of Sleep"
    difficulty
    [ ".               .               .               seventySteps     .                ."
    , ".               .               .               .                theCavernOfFlame ."
    , ".               .               .               .                .                sevenHundredSteps"
    , ".               .               .               .                baseOfTheSteps   ."
    , ".               .               .               theEnchantedPath .                ."
    , ".               enchantedWoods1 .               .                .                enchantedWoods2"
    , ".               enchantedWoods3 .               .                .                enchantedWoods4"
    , ".               .               enchantedWoods5 .                enchantedWoods6  ."
    ]
    $ (metaL .~ toJSON ([] :: [Dream]))
    . (hasEncounterDeckL .~ False)

data Dream
  = GuardianDream
  | SeekerDream
  | RogueDream
  | MysticDream
  | SurvivorDream
  | CriminalDream
  | DrifterDream
  | HunterDream
  | MedicOrAssistantDream
  | MiskatonicOrScholarDream
  | VeteranDream
  | WayfarerDream
  | NeutralDream1
  | NeutralDream2
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

dreamLabel :: Dream -> Text
dreamLabel = \case
  GuardianDream -> "Guardian Dream"
  SeekerDream -> "Seeker Dream"
  RogueDream -> "Rogue Dream"
  MysticDream -> "Mystic Dream"
  SurvivorDream -> "Survivor Dream"
  CriminalDream -> "Criminal Dream"
  DrifterDream -> "Drifter Dream"
  HunterDream -> "Hunter Dream"
  MedicOrAssistantDream -> "Medic or Assistant Dream"
  MiskatonicOrScholarDream -> "Miskatonic or Scholar Dream"
  VeteranDream -> "Veteran Dream"
  WayfarerDream -> "Wayfarer Dream"
  NeutralDream1 -> "Neutral Dream 1"
  NeutralDream2 -> "Neutral Dream 2"

dreamEffect :: HasGame m => InvestigatorId -> Dream -> m Message
dreamEffect iid = \case
  GuardianDream -> pure $ search iid GameSource iid [fromDeck] (CardWithClass Guardian) (PlayFound iid 1)
  SeekerDream -> pure $ search iid GameSource iid [fromDeck] (CardWithClass Seeker) (PlayFound iid 1)
  RogueDream -> pure $ search iid GameSource iid [fromDeck] (CardWithClass Rogue) (PlayFound iid 1)
  MysticDream -> pure $ search iid GameSource iid [fromDeck] (CardWithClass Mystic) (PlayFound iid 1)
  SurvivorDream -> pure $ search iid GameSource iid [fromDeck] (CardWithClass Survivor) (PlayFound iid 1)
  CriminalDream ->
    pure
      $ search
        iid
        GameSource
        iid
        [fromDeck]
        (oneOf [CardWithTrait Criminal, CardWithTrait Illicit])
        (PlayFound iid 1)
  DrifterDream ->
    pure
      $ search
        iid
        GameSource
        iid
        [fromDeck]
        BasicWeaknessCard
        (DeferSearchedToTarget $ LabeledTarget "Drifter" ScenarioTarget)
  HunterDream -> pure $ search iid GameSource iid [fromDeck] (CardWithTrait Weapon) (PlayFound iid 1)
  MedicOrAssistantDream -> do
    otherInvestigators <- selectList $ NotInvestigator $ InvestigatorWithId iid
    player <- getPlayer iid
    pure
      $ chooseOne
        player
        [ targetLabel
          investigator
          [ takeResources investigator ScenarioSource 2
          , setupModifier ScenarioSource investigator $ StartingHand 1
          ]
        | investigator <- otherInvestigators
        ]
  MiskatonicOrScholarDream -> pure $ search iid GameSource iid [fromDeck] (CardWithTrait Tome) (PlayFound iid 1)
  VeteranDream ->
    pure
      $ search
        iid
        GameSource
        iid
        [fromDeck]
        (oneOf [CardWithTrait Tactic, CardWithTrait Supply])
        (DeferSearchedToTarget $ LabeledTarget "Veteran" ScenarioTarget)
  WayfarerDream ->
    pure
      $ search
        iid
        GameSource
        iid
        [fromDeck]
        (oneOf [CardWithTrait Wayfarer, CardWithTrait Relic])
        (PlayFound iid 1)
  NeutralDream1 -> do
    pure $ takeResources iid ScenarioSource 2
  NeutralDream2 -> do
    pure $ setupModifier ScenarioSource iid $ StartingHand 1

dreamsMap :: Map Dream FlavorText
dreamsMap =
  mapFromList
    [ (GuardianDream, guardianDream)
    , (SeekerDream, seekerDream)
    , (RogueDream, rogueDream)
    , (MysticDream, mysticDream)
    , (SurvivorDream, survivorDream)
    , (CriminalDream, criminalDream)
    , (DrifterDream, drifterDream)
    , (HunterDream, hunterDream)
    , (MedicOrAssistantDream, medicOrAssistantDream)
    , (MiskatonicOrScholarDream, miskatonicOrScholarDream)
    , (VeteranDream, veteranDream)
    , (WayfarerDream, wayfarerDream)
    , (NeutralDream1, neutralDream1)
    , (NeutralDream2, neutralDream2)
    ]

classDreams :: ClassSymbol -> [Dream]
classDreams Guardian = [GuardianDream, NeutralDream1, NeutralDream2]
classDreams Seeker = [SeekerDream, NeutralDream1, NeutralDream2]
classDreams Rogue = [RogueDream, NeutralDream1, NeutralDream2]
classDreams Mystic = [MysticDream, NeutralDream1, NeutralDream2]
classDreams Survivor = [SurvivorDream, NeutralDream1, NeutralDream2]
classDreams Neutral = [NeutralDream1, NeutralDream2]
classDreams Mythos = []

traitsDreams :: [Trait] -> [Dream]
traitsDreams = nub . concatMap traitDreams
 where
  traitDreams = \case
    Criminal -> [CriminalDream]
    Drifter -> [DrifterDream]
    Hunter -> [HunterDream]
    Medic -> [MedicOrAssistantDream]
    Assistant -> [MedicOrAssistantDream]
    Miskatonic -> [MiskatonicOrScholarDream]
    Scholar -> [MiskatonicOrScholarDream]
    Veteran -> [VeteranDream]
    Wayfarer -> [WayfarerDream]
    _ -> []

instance HasChaosTokenValue BeyondTheGatesOfSleep where
  getChaosTokenValue iid tokenFace (BeyondTheGatesOfSleep attrs) = case tokenFace of
    Skull -> do
      cards <- fieldMap InvestigatorHand length iid
      pure $ toChaosTokenValue attrs Skull (ceiling @Double $ fromIntegral cards / 2.0) cards
    Cultist -> do
      easyStandard <- selectCount $ LocationWithTitle "Enchanted Woods"
      hardExpert <- selectCount $ LocationWithTrait Woods
      pure $ toChaosTokenValue attrs Cultist easyStandard hardExpert
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BeyondTheGatesOfSleep where
  runMessage msg s@(BeyondTheGatesOfSleep attrs) = case msg of
    DrawStartingHands -> do
      void $ runMessage msg attrs
      iids <- allInvestigators
      pushAll $ map (`ForInvestigator` Setup) iids
      pure s
    ForInvestigator i Setup -> do
      let
        usedDreams = case fromJSON (scenarioMeta attrs) of
          Error e -> error $ "failed to parse dreams: " <> e
          Success result -> result
      investigatorClass <- field InvestigatorClass i
      traits <- field InvestigatorTraits i
      player <- getPlayer i
      players <- allPlayers

      let
        allDreams = classDreams investigatorClass <> traitsDreams (toList traits)
        unusedDreams = allDreams \\ usedDreams
        availableDreams = if null unusedDreams then allDreams else unusedDreams
        chooseDream dream =
          Label
            (dreamLabel dream)
            . ( [ story players $ findWithDefault (error "missing dream") dream dreamsMap
                , SetScenarioMeta $ toJSON $ dream : usedDreams
                ]
                  <>
              )
            . pure
            <$> dreamEffect i dream

      choices <- traverse chooseDream availableDreams

      push $ chooseOne player choices
      pure s
    Setup -> do
      _encounterDeck <-
        buildEncounterDeck
          [ EncounterSet.BeyondTheGatesOfSleep
          , EncounterSet.AgentsOfNyarlathotep
          , EncounterSet.Zoogs
          , EncounterSet.DreamersCurse
          , EncounterSet.Dreamlands
          , EncounterSet.ChillingCold
          ]

      (seventySteps, placeSeventySteps) <- placeLocationCard Locations.seventySteps
      placeTheCavernOfFlame <- placeLocationCard_ Locations.theCavernOfFlame

      setAsideCards <-
        genCards
          [ Enemies.nasht
          , Enemies.kamanThah
          , Locations.sevenHundredSteps
          , Locations.baseOfTheSteps
          , Locations.theEnchantedPath
          , Locations.enchantedWoodsMysticalForest
          , Locations.enchantedWoodsVillageOfZoogs
          , Locations.enchantedWoodsGreatStoneCircle
          , Locations.enchantedWoodsStoneTrapdoor
          , Locations.enchantedWoodsTheMoonTree
          , Locations.enchantedWoodsFungalForest
          , Locations.enchantedWoodsLostWoods
          ]

      pushAll
        $ [ SetAgendaDeck
          , SetActDeck
          , placeSeventySteps
          , placeTheCavernOfFlame
          , MoveAllTo (toSource attrs) seventySteps
          ]

      agendas <- genCards [Agendas.journeyThroughTheGates]
      acts <-
        genCards
          [Acts.enteringTheDreamlands, Acts.theTrialOfNashtAndKamanThah, Acts.theFinalDescent, Acts.thePath]

      BeyondTheGatesOfSleep
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    SearchFound iid (LabeledTarget "Drifter" ScenarioTarget) _ cards | notNull cards -> do
      player <- getPlayer iid
      playerCount <- getPlayerCount
      let
        weaknessFilter =
          if playerCount < 2
            then notElem MultiplayerOnly . cdDeckRestrictions
            else const True
      newWeakness <- genCard =<< sample (NE.fromList $ filter weaknessFilter allBasicWeaknesses)
      pushAll
        [ FocusCards cards
        , questionLabel "Replace basic weakness with random and take 1 trauma of your choice?" player
            $ ChooseOne
            $ Label "Do not replace" [UnfocusCards]
            : [ targetLabel
                (toCardId card)
                [ UnfocusCards
                , RemoveCardFromSearch iid (toCardId card)
                , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [newWeakness]
                , chooseOne
                    player
                    [ Label "Take 1 Physical trauma" [SufferTrauma iid 1 0]
                    , Label "Take 1 Mental trauma" [SufferTrauma iid 0 1]
                    ]
                ]
              | card <- cards
              ]
        ]
      pure s
    SearchFound _ (LabeledTarget "Veteran" ScenarioTarget) _ _ -> do
      push $ DoStep 2 msg
      pure s
    DoStep n (SearchFound iid (LabeledTarget "Veteran" ScenarioTarget) deck cards) | notNull cards -> do
      player <- getPlayer iid
      pushAll
        [ FocusCards cards
        , questionLabel
            ( "Choose up to "
                <> tshow n
                <> " Tactic and/or Supply cards and begin this scenario with them as additional cards in your opening hand."
            )
            player
            $ ChooseOne
            $ Label "Do not take any" [UnfocusCards]
            : [ targetLabel
                (toCardId card)
                $ [ UnfocusCards
                  , RemoveCardFromSearch iid (toCardId card)
                  , setupModifier ScenarioSource iid (AdditionalStartingCards [card])
                  ]
                <> [ DoStep 1 (SearchFound iid (LabeledTarget "Veteran" ScenarioTarget) deck (deleteFirst card cards))
                   | n > 1
                   ]
              | card <- cards
              ]
        ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      when (isEasyStandard attrs) $ do
        mtarget <- getSkillTestTarget
        maction <- getSkillTestAction
        case (maction, mtarget) of
          (Just action, Just (EnemyTarget eid)) | action `elem` [#fight, #evade] -> do
            isSwarming <- eid <=~> SwarmingEnemy
            pushWhen isSwarming $ PlaceSwarmCards iid eid 1
          _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) $ do
        mtarget <- getSkillTestTarget
        maction <- getSkillTestAction
        case (maction, mtarget) of
          (Just action, Just (EnemyTarget eid)) | action `elem` [#fight, #evade] -> do
            isSwarming <- eid <=~> SwarmingEnemy
            pushWhen isSwarming $ PlaceSwarmCards iid eid 1
          _ -> pure ()
      pure s
    _ -> BeyondTheGatesOfSleep <$> runMessage msg attrs
