module Arkham.Scenario.Scenarios.RelicsOfThePast (relicsOfThePast) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign (getCampaignStoryCards, matchingCardsAlreadyInDeck)
import Arkham.Helpers.Card (ConvertToCard, convertToCard, getVictoryPoints)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getLocationOf, withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorName))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.RelicsOfThePast.Helpers
import Arkham.Tracing
import Arkham.Trait (Trait (Ancient, Serpent))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Arkham.Xp

newtype RelicsOfThePast = RelicsOfThePast ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicsOfThePast :: Difficulty -> RelicsOfThePast
relicsOfThePast difficulty =
  sideStory
    RelicsOfThePast
    "90065"
    "Relics of the Past"
    difficulty
    [ ".      square diamond  .        ."
    , "circle square diamond  squiggle hourglass"
    , "circle star   triangle squiggle hourglass"
    , ".      star   triangle .        ."
    ]

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusThree
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , Zero , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFive
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusTwo , MinusThree , MinusThree , MinusFour , MinusSix
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusThree , MinusFour , MinusFour
  , MinusSix , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

instance HasChaosTokenValue RelicsOfThePast where
  getChaosTokenValue iid chaosTokenFace (RelicsOfThePast attrs) = case chaosTokenFace of
    Skull -> do
      n <- selectCount LocationWithAnyDoom
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

moveNearestSerpentToward :: ReverseQueue m => InvestigatorId -> m ()
moveNearestSerpentToward iid = do
  enemies <- select $ NearestEnemyTo iid (ReadyEnemy <> UnengagedEnemy <> EnemyWithTrait Serpent)
  unless (null enemies) do
    chooseOrRunOneM iid $ targets enemies \enemy ->
      push $ MoveToward (toTarget enemy) (locationWithInvestigator iid)

hasCampaignCard :: (HasGame m, Tracing m) => InvestigatorId -> CardDef -> m Bool
hasCampaignCard iid def = do
  inDeck <-
    member (toCardCode def) . findWithDefault mempty iid <$> matchingCardsAlreadyInDeck (cardIs def)
  storyCards <- findWithDefault [] iid <$> getCampaignStoryCards
  pure $ inDeck || any ((== def) . toCardDef) storyCards

isMontereyJack :: (HasGame m, Tracing m) => InvestigatorId -> m Bool
isMontereyJack = fieldMap InvestigatorName ((== "Monterey Jack") . toTitle)

toVictoryEntries :: (ConvertToCard c, HasGame m, Tracing m) => [c] -> m [(Text, Int)]
toVictoryEntries = mapMaybeM \c -> do
  card <- convertToCard c
  mPoints <- getVictoryPoints card
  pure $ (toTitle (RevealedCard card),) <$> mPoints

instance RunMessage RelicsOfThePast where
  runMessage msg s@(RelicsOfThePast attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      scope "supplies" $ flavor $ h "title" >> p "body"
      investigators <- allInvestigators
      let extraPick = length investigators == 1
      for_ investigators \iid -> do
        push $ ForInvestigator iid (ScenarioSpecific "pickSupply" Null)
        when extraPick $ push $ ForInvestigator iid (ScenarioSpecific "pickSupply" Null)
      pure s
    ForInvestigator iid (ScenarioSpecific "pickSupply" _) -> do
      let pickedSupplies = toResultDefault @(Map InvestigatorId [Supply]) mempty attrs.meta
      let available = filter (`notElem` concat (toList pickedSupplies)) scenarioSupplies
      chooseOneM iid $ scenarioI18n do
        questionLabeled' "supplies.question"
        unscoped $ labeled' "skip" nothing
        for_ available \supply ->
          labeled' (supplyKey supply)
            $ push
            $ ForInvestigator iid (ScenarioSpecific "pickedSupply" (toJSON supply))
      pure s
    ForInvestigator iid (ScenarioSpecific "pickedSupply" v) -> do
      let supply = toResult @Supply v
      pickSupply iid supply
      let pickedSupplies = toResultDefault @(Map InvestigatorId [Supply]) mempty attrs.meta
      pure . RelicsOfThePast $ attrs & metaL .~ toJSON (insertWith (<>) iid [supply] pickedSupplies)
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup RelicsOfThePast attrs do
      setup $ ul do
        li "gatherSets"
        li "removeCards"
        li "explorationDeck"
        li "setAside"
        li "placeEntryway"
        li "poisoned"
        unscoped $ li "shuffleRemainder"

      gather Set.RelicsOfThePast
      gather Set.TheDoomOfEztli
      gather Set.AgentsOfYig
      gather Set.DeadlyTraps
      gather Set.ForgottenRuins
      gather Set.Poison
      gather Set.Serpents
      gather Set.YigsVenom
      gather Set.ChillingCold
      gather Set.LockedDoors
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      removeEvery
        [ Locations.secretPassage
        , Locations.ancientHall
        , Locations.chamberOfTime
        , Assets.relicOfAgesADeviceOfSomeSort
        , Enemies.harbingerOfValusia
        ]

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      setAside
        $ [ Locations.innerChamber
          , Treacheries.vengeantPast
          , Enemies.broodOfYig
          , Enemies.broodOfYig
          , Enemies.broodOfYig
          , Assets.jadeCrocodile
          , Assets.obsidianJaguar
          , Assets.citrineSnake
          , Assets.turquoiseEagle
          ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      setActDeck [Acts.crumblingRuin, Acts.findTheWayOut]
      setAgendaDeck [Agendas.somethingElseStirs, Agendas.guardianOfTheRelics]

      explorationDeck <-
        shuffle
          $ [ Locations.grandChamber
            , Locations.burialPit
            , Locations.undergroundRuins
            , Locations.secretPassageRelicsOfThePast
            , Locations.ancientHallRelicsOfThePast
            , Treacheries.deepDark
            , Treacheries.finalMistake
            , Enemies.pitViper
            , Enemies.pitViper
            , Enemies.pitViper
            ]
          <> (if isHardExpert attrs then [Treacheries.lockedDoor, Treacheries.entombed] else [])
      addExtraDeck ExplorationDeck explorationDeck

      entryway <- place Locations.entryway
      startAt entryway
    Explore iid source _ -> do
      mloc <- runMaybeT $ asum [hoistMaybe source.location, MaybeT $ getLocationOf iid]
      checkWhen $ Window.AttemptExplore iid mloc
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore
        iid
        source
        (CardWithOneOf [CardWithType EncounterAssetType, locationMatcher])
        PlaceExplored
        1
      pure s
    ResolveChaosToken _ Cultist iid -> do
      whenPoisoned iid $ push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        withLocationOf iid \lid -> placeDoom Tablet lid 1
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        afterSkillTestQuiet $ moveNearestSerpentToward iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case token.face of
        Tablet -> withLocationOf iid \lid -> placeDoom Tablet lid 1
        ElderThing -> moveNearestSerpentToward iid
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      let returnSupplies = do
            let pickedSupplies = toResultDefault @(Map InvestigatorId [Supply]) mempty attrs.meta
            for_ (mapToList pickedSupplies) \(iid, picked) -> for_ picked (useSupply iid)
      mMonterey <- selectOne $ IncludeEliminated $ InvestigatorWithTitle "Monterey Jack"
      case r of
        NoResolution -> do
          anyResigned <- selectAny $ IncludeEliminated ResignedInvestigator
          push $ if anyResigned then R1 else R2
        Resolution 1 -> do
          vd <- getVictoryDisplay
          let ancientRelics =
                filter (`cardMatch` (CardWithType EncounterAssetType <> CardWithTrait Ancient)) vd
          let enemyAndLocationCards =
                filter (`cardMatch` CardWithOneOf [CardWithType EnemyType, CardWithType LocationType]) vd
          locations <- select $ RevealedLocation <> LocationWithoutClues
          montereyEntries <- toVictoryEntries ancientRelics
          otherEntries <- (<>) <$> toVictoryEntries enemyAndLocationCards <*> toVictoryEntries locations

          pickedSupplies <- getPickedSupplies
          resigned <- select $ IncludeEliminated ResignedInvestigator
          let journalResigned =
                or
                  [ iid `elem` resigned
                  | (iid, picked) <- mapToList pickedSupplies
                  , Journal `elem` picked
                  ]
          let journalBonus = if journalResigned then 1 else 0
          let montereyXp = sum (map snd montereyEntries) + journalBonus
          let otherXp = sum (map snd otherEntries) + journalBonus

          resolutionFlavor
            $ withVars ["montereyXp" .= montereyXp, "xp" .= otherXp]
            $ setTitle "resolution1.title"
            >> p "resolution1.body"

          investigators <- select InvestigatorCanGainXp
          montereys <- filterM isMontereyJack investigators
          let journalLbl = scope "xp" $ "$" <> ikey "journal"
          let
            applyModifier n (XPModifier _ m) = max 0 (n + m)
            applyModifier n _ = n
          push
            $ ReportXp
            $ XpBreakdown
            $ concat
              [ [ InvestigatorGainXp iid $ XpDetail XpFromVictoryDisplay lbl n
                | iid <- investigators
                , (lbl, n) <- if iid `elem` montereys then montereyEntries else otherEntries
                ]
              , [ InvestigatorGainXp iid $ XpDetail XpBonus journalLbl 1
                | journalResigned
                , iid <- investigators
                ]
              ]
          for_ investigators \iid -> do
            let base = if iid `elem` montereys then montereyXp else otherXp
            modifiers' <- getModifiers iid
            push $ GainXP iid (toSource attrs) (foldl' applyModifier base modifiers')

          for_ mMonterey \monterey -> do
            hasOriginalBullwhip <- hasCampaignCard monterey Assets.trustyBullwhip
            hasAdvancedSecrets <- hasCampaignCard monterey Treacheries.buriedSecretsAdvanced
            chooseOneM monterey $ scenarioI18n do
              questionLabeled' "swap.question"
              when hasOriginalBullwhip do
                labeled' "swap.upgradeTrustyBullwhip" do
                  removeCampaignCardFromDeck monterey Assets.trustyBullwhip
                  addCampaignCardToDeck monterey DoNotShuffleIn Assets.trustyBullwhipAdvanced
              when hasAdvancedSecrets do
                labeled' "swap.downgradeBuriedSecrets" do
                  removeCampaignCardFromDeck monterey Treacheries.buriedSecretsAdvanced
                  addCampaignCardToDeck monterey DoNotShuffleIn Treacheries.buriedSecrets
              labeled' "swap.doNotSwap" nothing

          returnSupplies
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs

          for_ mMonterey \monterey -> do
            hasOriginalSecrets <- hasCampaignCard monterey Treacheries.buriedSecrets
            hasAdvancedBullwhip <- hasCampaignCard monterey Assets.trustyBullwhipAdvanced
            when (hasOriginalSecrets || hasAdvancedBullwhip) do
              chooseOrRunOneM monterey $ scenarioI18n do
                questionLabeled' "swap.questionMust"
                when hasOriginalSecrets do
                  labeled' "swap.upgradeBuriedSecrets" do
                    removeCampaignCardFromDeck monterey Treacheries.buriedSecrets
                    addCampaignCardToDeck monterey DoNotShuffleIn Treacheries.buriedSecretsAdvanced
                when hasAdvancedBullwhip do
                  labeled' "swap.downgradeTrustyBullwhip" do
                    removeCampaignCardFromDeck monterey Assets.trustyBullwhipAdvanced
                    addCampaignCardToDeck monterey DoNotShuffleIn Assets.trustyBullwhip

          returnSupplies
          endOfScenario
        _ -> error "Unknown Resolution"
      pure s
    _ -> RelicsOfThePast <$> liftRunMessage msg attrs
