module Arkham.Scenario.Scenarios.RedTideRising (redTideRising) where

import Arkham.Id
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Card (getVictoryPoints)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query (getInvestigators, getLead, getPlayerCount)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget)
import Arkham.Helpers.Xp (toGainXp)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (..))
import Arkham.Placement (Placement (AtLocation))
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.RedTideRising.Helpers
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers (
  getLeadsDeck,
  hideouts,
  shuffleIntoLeadsDeck,
  shuffleLeadsDeck,
  suspects,
 )
import Arkham.Trait (Trait (Hideout, Suspect))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Xp

newtype RedTideRising = RedTideRising ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- FOURMOLU_DISABLE -}
redTideRising :: Difficulty -> RedTideRising
redTideRising difficulty =
  sideStory
    RedTideRising
    "90041"
    "Red Tide Rising"
    difficulty
    [ "esotericOrderOfDagon .                    newChurchGreen  .                theHouseOnWaterStreet"
    , "esotericOrderOfDagon firstNationalGrocery newChurchGreen  marshRefinery    theHouseOnWaterStreet"
    , "theLittleBookshop    firstNationalGrocery innsmouthSquare marshRefinery    innsmouthHarbour"
    , "theLittleBookshop    gilmanHouse          innsmouthSquare fishStreetBridge innsmouthHarbour"
    , "sawboneAlley         gilmanHouse          innsmouthJail   fishStreetBridge shorewardSlums"
    , "sawboneAlley         .                    innsmouthJail   .                shorewardSlums"
    ]

easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , MinusSix , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix , MinusSeven
  , MinusEight , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

instance HasChaosTokenValue RedTideRising where
  getChaosTokenValue iid tokenFace (RedTideRising attrs) = case tokenFace of
    Skull -> do
      n <-
        selectCount
          $ VictoryDisplayCardMatch
          $ basic
          $ oneOf [CardWithTrait Suspect, CardWithTrait Hideout]
      pure $ toChaosTokenValue attrs Skull ((n + 1) `div` 2) n
    Cultist -> do
      followingLead <-
        getSkillTestAction >>= \case
          Just action
            | action == #parley ->
                getSkillTestTarget >>= \case
                  Just ((.enemy) -> Just eid) -> eid <=~> EnemyWithTrait Suspect
                  _ -> pure False
            | action == #investigate ->
                getSkillTestTarget >>= \case
                  Just ((.location) -> Just lid) -> lid <=~> LocationWithTrait Hideout
                  _ -> pure False
          _ -> pure False
      pure
        $ if followingLead
          then toChaosTokenValue attrs Cultist 4 5
          else toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RedTideRising where
  runMessage msg s@(RedTideRising attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup RedTideRising attrs do
      setup $ ul do
        li "gatherSets"
        li "midnightMasks"
        li "scenarioReference"
        li "actAgendaDeck"
        li "placeLocations"
        li "leadsDeck"
        li "drawLead"
        li "setAsideMonsters"
        li "setAsideAngryMob"
        li "adjustments"
        li "removeStoryAssets"
        li "mysteriousPhoto"
        li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.RedTideRising
      gather Set.TheVanishingOfElinaHarper
      gather Set.AgentsOfDagon
      gather Set.FogOverInnsmouth
      gather Set.TheLocals
      gather Set.ChillingCold
      gather Set.LockedDoors
      gather Set.Nightgaunts
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      setAgendaDeck [Agendas.theNewGirl, Agendas.trailGoesCold]
      setActDeck [Acts.searchingForDad]

      removeEvery [Assets.thomasDawsonSoldierInANewWar, Assets.elinaHarperKnowsTooMuch]

      innsmouthSquare <- place Locations.innsmouthSquare
      marshRefinery <- place Locations.marshRefinery
      innsmouthHarbour <- place Locations.innsmouthHarbour
      fishStreetBridge <- place Locations.fishStreetBridge
      firstNationalGrocery <- place Locations.firstNationalGrocery
      gilmanHouse <- place Locations.gilmanHouse
      theLittleBookshop <- place Locations.theLittleBookshop

      playerCount <- getPlayerCount
      let
        adjusted = playerCount + case attrs.difficulty of
          Hard -> 1
          Expert -> 2
          _ -> 0
        monsters =
          [ Enemies.wingedOneFogOverInnsmouth
          , Enemies.huntingNightgaunt
          , Enemies.huntingNightgaunt
          ]

      mExtraMonster <-
        if adjusted >= 4
          then Just <$> sample (Enemies.wingedOneFogOverInnsmouth :| [Enemies.huntingNightgaunt, Enemies.huntingNightgaunt])
          else pure Nothing

      placeDoomOnAgenda $ case adjusted of
        2 -> 1
        3 -> 2
        5 -> 1
        6 -> 2
        _ -> 0

      (firstLead, remainingLeads) <- sampleWithRest (suspects <> hideouts)
      addExtraDeck LeadsDeck =<< shuffle (remainingLeads <> maybeToList mExtraMonster)

      setAside $ Enemies.angryMob : maybe monsters (`deleteFirst` monsters) mExtraMonster

      let
        spawnLocationFor suspect
          | suspect == Enemies.brianBurnhamWantsOut = firstNationalGrocery
          | suspect == Enemies.otheraGilmanProprietessOfTheHotel = gilmanHouse
          | suspect == Enemies.joyceLittleBookshopOwner = theLittleBookshop
          | suspect == Enemies.barnabasMarshTheChangeIsUponHim = marshRefinery
          | suspect == Enemies.zadokAllenDrunkAndDisorderly = fishStreetBridge
          | suspect == Enemies.robertFriendlyDisgruntledDockworker = innsmouthHarbour
          | otherwise = innsmouthSquare

      startLocation <-
        if firstLead `elem` toList hideouts
          then place firstLead
          else do
            let spawnLocation = spawnLocationFor firstLead
            enemy <- enemyAt firstLead spawnLocation
            placeClues ScenarioSource enemy =<< perPlayer 1
            pure spawnLocation

      wendy <- maybe getLead pure =<< getWendyAdams
      reveal startLocation
      reveal innsmouthSquare
      investigators <- getInvestigators
      for_ investigators \iid ->
        push $ PlaceInvestigator iid (AtLocation $ if iid == wendy then startLocation else innsmouthSquare)

      photo <- fromGathered1 Assets.mysteriousPhoto
      lift $ chooseOneM wendy $ scenarioI18n do
        labeled' "mysteriousPhotoSuspects" $ push $ TakeControlOfSetAsideAsset wendy photo
        labeled' "mysteriousPhotoHideouts" $ push $ TakeControlOfSetAsideAsset wendy (flipCard photo)
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Tablet) do
        cards <- take 1 <$> getLeadsDeck
        unless (null cards) do
          focusCards cards do
            chooseOneM iid $ scenarioI18n do
              labeled' "shuffleLeadsDeck" shuffleLeadsDeck
              labeled' "doNotShuffleLeadsDeck" nothing
      pure s
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == ElderThing) do
        cards <- take 1 . unDeck <$> getEncounterDeck
        shuffleIntoLeadsDeck cards
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          anyDefeated <- selectAny DefeatedInvestigator
          resolution "noResolution"
          push $ if anyDefeated then R2 else R1
        Resolution 1 -> do
          resolution "resolution1"
          gainCustomXp attrs
          wendyDefeated <- selectAny $ IncludeEliminated (wendyAdams <> DefeatedInvestigator)
          if wendyDefeated
            then mustSwapReward
            else maySwapReward
          endOfScenario
        Resolution 2 -> do
          resolution "resolution2"
          gainCustomXp attrs
          mustSwapReward
          endOfScenario
        _ -> throw $ UnknownResolution r
      pure s
    _ -> RedTideRising <$> liftRunMessage msg attrs

gainCustomXp :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m ()
gainCustomXp attrs = do
  victoryDisplay <- scenarioField ScenarioVictoryDisplay
  let isLead card = card `cardMatch` oneOf [CardWithTrait Suspect, CardWithTrait Hideout]
  let total = length victoryDisplay
  otherVictory <-
    sum . catMaybes <$> traverse getVictoryPoints (filter (not . isLead) victoryDisplay)
  investigators <- select InvestigatorCanGainXp
  details <- for investigators \iid -> do
    isWendy <- iid <=~> IncludeEliminated wendyAdams
    let base = otherVictory + max 0 (total - (if isWendy then 3 else 5))
    mods <- getModifiers iid
    let applyModifier n (XPModifier _ m) = max 0 (n + m)
        applyModifier n _ = n
    pure (iid, isWendy, foldl' applyModifier base mods)
  push
    $ ReportXp
    $ XpBreakdown
      [ InvestigatorGainXp iid
        $ XpDetail XpFromVictoryDisplay ("$" <> ikey (if isWendy then "xp.wendy" else "xp.other")) n
      | (iid, isWendy, n) <- details
      ]
  pushAll =<< toGainXp attrs (pure [(iid, n) | (iid, _, n) <- details])

swapCards
  :: ReverseQueue m => InvestigatorId -> CardDef -> CardDef -> m ()
swapCards wendy fromDef toDef = do
  removeCampaignCardFromDeck wendy fromDef
  addCampaignCardToDeck wendy DoNotShuffleIn toDef

mustSwapReward :: (HasI18n, ReverseQueue m) => m ()
mustSwapReward = do
  mWendy <- selectOne (IncludeEliminated wendyAdams)
  for_ mWendy \wendy -> do
    canUpgradeWeakness <- ownsCard wendy Treacheries.abandonedAndAlone
    canDowngradeAmulet <- ownsCard wendy Assets.wendysAmuletAdvanced
    when (canUpgradeWeakness || canDowngradeAmulet) do
      chooseOrRunOneM wendy do
        when canUpgradeWeakness do
          labeled' "upgradeAbandonedAndAlone"
            $ swapCards wendy Treacheries.abandonedAndAlone Treacheries.abandonedAndAloneAdvanced
        when canDowngradeAmulet do
          labeled' "downgradeWendysAmulet"
            $ swapCards wendy Assets.wendysAmuletAdvanced Assets.wendysAmulet

maySwapReward :: (HasI18n, ReverseQueue m) => m ()
maySwapReward = do
  mWendy <- selectOne (IncludeEliminated wendyAdams)
  for_ mWendy \wendy -> do
    canUpgradeAmulet <- ownsCard wendy Assets.wendysAmulet
    canDowngradeWeakness <- ownsCard wendy Treacheries.abandonedAndAloneAdvanced
    when (canUpgradeAmulet || canDowngradeWeakness) do
      chooseOneM wendy do
        when canUpgradeAmulet do
          labeled' "upgradeWendysAmulet"
            $ swapCards wendy Assets.wendysAmulet Assets.wendysAmuletAdvanced
        when canDowngradeWeakness do
          labeled' "downgradeAbandonedAndAlone"
            $ swapCards wendy Treacheries.abandonedAndAloneAdvanced Treacheries.abandonedAndAlone
        labeled' "doNotSwap" nothing

ownsCard :: ReverseQueue m => InvestigatorId -> CardDef -> m Bool
ownsCard iid def = selectAny $ OwnedBy (InvestigatorWithId iid) <> basic (cardIs def)
