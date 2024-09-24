module Arkham.Scenario.Scenarios.BeyondTheGatesOfSleep (
  BeyondTheGatesOfSleep (..),
  beyondTheGatesOfSleep,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDreamEaters.ChaosBag
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers hiding (setupModifier)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (hasEncounterDeckL, metaL)
import Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText
import Arkham.Strategy
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
    [ ".               seventySteps      ."
    , ".               .                 theCavernOfFlame"
    , ".               sevenHundredSteps ."
    , ".               .                baseOfTheSteps"
    , "enchantedWoods1 theEnchantedPath enchantedWoods2"
    , "enchantedWoods3 .                enchantedWoods4"
    , "enchantedWoods5 .                enchantedWoods6"
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

dreamEffect :: ReverseQueue m => InvestigatorId -> Dream -> m ()
dreamEffect iid = \case
  GuardianDream -> search iid GameSource iid [fromDeck] #guardian (PlayFound iid 1)
  SeekerDream -> search iid GameSource iid [fromDeck] #seeker (PlayFound iid 1)
  RogueDream -> search iid GameSource iid [fromDeck] #rogue (PlayFound iid 1)
  MysticDream -> search iid GameSource iid [fromDeck] #mystic (PlayFound iid 1)
  SurvivorDream -> search iid GameSource iid [fromDeck] #survivor (PlayFound iid 1)
  CriminalDream ->
    search iid GameSource iid [fromDeck] (basic $ oneOf $ withTrait <$> [Criminal, Illicit])
      $ PlayFound iid 1
  DrifterDream ->
    search iid GameSource iid [fromDeck] (basic BasicWeaknessCard)
      $ defer (LabeledTarget "Drifter" ScenarioTarget) IsNotDraw
  HunterDream -> search iid GameSource iid [fromDeck] (basic $ withTrait Weapon) (PlayFound iid 1)
  MedicOrAssistantDream -> do
    otherInvestigators <- select $ NotInvestigator $ InvestigatorWithId iid
    chooseTargetM iid otherInvestigators \investigator -> do
      gainResourcesIfCan investigator ScenarioSource 2
      setupModifier ScenarioSource investigator $ StartingHand 1
  MiskatonicOrScholarDream -> search iid GameSource iid [fromDeck] (basic $ withTrait Tome) (PlayFound iid 1)
  VeteranDream ->
    search iid GameSource iid [fromDeck] (basic $ oneOf $ withTrait <$> [Tactic, Supply])
      $ defer (LabeledTarget "Veteran" ScenarioTarget) IsNotDraw
  WayfarerDream ->
    search iid GameSource iid [fromDeck] (basic $ oneOf $ withTrait <$> [Wayfarer, Relic])
      $ PlayFound iid 1
  NeutralDream1 -> gainResourcesIfCan iid ScenarioSource 2
  NeutralDream2 -> setupModifier ScenarioSource iid $ StartingHand 1

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
      easyStandard <- selectCount $ RevealedLocation <> LocationWithTitle "Enchanted Woods"
      hardExpert <- selectCount $ RevealedLocation <> LocationWithTrait Woods
      pure $ toChaosTokenValue attrs Cultist easyStandard hardExpert
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BeyondTheGatesOfSleep where
  runMessage msg s@(BeyondTheGatesOfSleep attrs) = runQueueT $ case msg of
    DrawStartingHands -> do
      void $ liftRunMessage msg attrs
      eachInvestigator \iid -> push $ ForInvestigator iid Setup
      pure s
    ForInvestigator i Setup -> do
      let
        usedDreams = case fromJSON attrs.meta of
          Error e -> error $ "failed to parse dreams: " <> e
          Success result -> result
      investigatorClass <- field InvestigatorClass i
      traits <- field InvestigatorTraits i

      let
        allDreams = classDreams investigatorClass <> traitsDreams (toList traits)
        unusedDreams = allDreams \\ usedDreams
        availableDreams = if null unusedDreams then allDreams else unusedDreams
        chooseDream dream = labeled (dreamLabel dream) do
          story $ findWithDefault (error "missing dream") dream dreamsMap
          push $ SetScenarioMeta $ toJSON $ dream : usedDreams
          dreamEffect i dream

      chooseOneM i do
        traverse chooseDream availableDreams
      pure s
    StandaloneSetup -> do
      setChaosTokens (initChaosBag TheDreamQuest attrs.difficulty)
      pure s
    Setup -> runScenarioSetup BeyondTheGatesOfSleep attrs do
      startAt =<< place Locations.seventySteps
      place_ Locations.theCavernOfFlame

      setAside
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

      setAgendaDeck [Agendas.journeyThroughTheGates]
      setActDeck
        [Acts.enteringTheDreamlands, Acts.theTrialOfNashtAndKamanThah, Acts.theFinalDescent, Acts.thePath]
    SearchFound iid (LabeledTarget "Drifter" ScenarioTarget) _ cards | notNull cards -> do
      playerCount <- getPlayerCount
      let
        weaknessFilter =
          if playerCount < 2
            then notElem MultiplayerOnly . cdDeckRestrictions
            else const True
      newWeakness <- genCard =<< sample (NE.fromList $ filter weaknessFilter allBasicWeaknesses)
      focusCards cards \unfocus -> do
        chooseOneM iid do
          questionLabeled "Replace basic weakness with random and take 1 trauma of your choice?"
          labeled "Do not replace" $ push unfocus
          targets cards \card -> do
            push unfocus
            push $ RemoveCardFromSearch iid (toCardId card)
            shuffleCardsIntoDeck iid [newWeakness]
            chooseOneM iid do
              labeled "Take 1 Physical trauma" $ sufferPhysicalTrauma iid 1
              labeled "Take 1 Mental trauma" $ sufferMentalTrauma iid 1
      pure s
    SearchFound _ (LabeledTarget "Veteran" ScenarioTarget) _ _ -> do
      doStep 2 msg
      pure s
    DoStep n (SearchFound iid (LabeledTarget "Veteran" ScenarioTarget) deck cards) | notNull cards -> do
      focusCards cards \unfocus -> do
        chooseOneM iid do
          questionLabeled
            $ "Choose up to "
            <> tshow n
            <> " Tactic and/or Supply cards and begin this scenario with them as additional cards in your opening hand."
          labeled "Do not take any" $ push unfocus
          targets cards \card -> do
            push unfocus
            push $ RemoveCardFromSearch iid (toCardId card)
            setupModifier ScenarioSource iid (AdditionalStartingCards [card])
            when (n > 1) do
              doStep 1 (SearchFound iid (LabeledTarget "Veteran" ScenarioTarget) deck (deleteFirst card cards))
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
    ScenarioResolution resolution -> do
      record $ case resolution of
        NoResolution -> TheInvestigatorsWereSavedByRandolphCarder
        Resolution 1 -> TheCatsCollectedTheirTributeFromTheZoogs
        Resolution 2 -> TheInvestigatorsParleyedWithTheZoogs
        _ -> error "Invalid Resolution"

      investigators <- allInvestigators
      addCampaignCardToDeckChoice investigators Assets.randolphCarterExpertDreamer
      allGainXp attrs
      endOfScenario
      pure s
    _ -> BeyondTheGatesOfSleep <$> liftRunMessage msg attrs
