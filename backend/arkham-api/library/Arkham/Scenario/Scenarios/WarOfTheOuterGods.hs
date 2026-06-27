module Arkham.Scenario.Scenarios.WarOfTheOuterGods (warOfTheOuterGods) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EnemyAttacks)
import Arkham.Message (CanAdvance (..), GroupKey (..))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.SkillTest.Base (skillTestCommittedCards)
import Arkham.Trait (Trait (Hex, Insect, Mutated))

newtype WarOfTheOuterGods = WarOfTheOuterGods ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warOfTheOuterGods :: Difficulty -> WarOfTheOuterGods
warOfTheOuterGods difficulty =
  scenarioWith
    WarOfTheOuterGods
    "86001"
    "War of the Outer Gods"
    difficulty
    [ ".    .    .      equals equals    heart     heart .     .            ."
    , ".    .    .      equals equals    heart     heart .     .            ."
    , ".    .    .      .      hourglass hourglass .     .     .            ."
    , ".    .    .      .      hourglass hourglass .     .     .            ."
    , ".    .    .      .      triangle  triangle  .     .     hubDimension hubDimension"
    , ".    .    circle circle .         .         star  star  .            ."
    , "moon moon spade  spade  .         .         t     t     droplet      droplet"
    ]
    (decksLayoutL .~ ["agenda1 .", "agenda2 act1", "agenda3 ."])

instance HasChaosTokenValue WarOfTheOuterGods where
  getChaosTokenValue iid chaosTokenFace (WarOfTheOuterGods attrs) = case chaosTokenFace of
    Skull -> do
      clues <- field InvestigatorClues iid
      let multiplier = if isEasyStandard attrs then 1 else 2
      pure $ ChaosTokenValue Skull (NegativeModifier $ clues * multiplier)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy -> [PlusOne, PlusOne, Zero, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo] <> base
  Standard ->
    [PlusOne, Zero, Zero, MinusOne, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour]
      <> base
  Hard ->
    [Zero, Zero, MinusOne, MinusOne, MinusTwo, MinusTwo, MinusThree, MinusFour, MinusFive, MinusSix]
      <> base
  Expert ->
    [Zero, MinusOne, MinusTwo, MinusThree, MinusFour, MinusFive, MinusSix, MinusSeven, MinusEight]
      <> base
 where
  base = [Skull, Skull, Skull, AutoFail, ElderSign]

{- | Place a card committed to this test facedown underneath an [[Insect]]
enemy, as a swarm card.
-}
commitSwarmCards :: ReverseQueue m => InvestigatorId -> Bool -> m ()
commitSwarmCards iid allOfThem = do
  insects <- select $ EnemyWithTrait Insect <> not_ IsSwarm
  unless (null insects) do
    whenJustM getSkillTest \st -> do
      let cards = concat (toList $ skillTestCommittedCards st)
      if allOfThem
        then for_ cards \card ->
          chooseTargetM iid insects (`placeCardAsSwarm` card)
        else when (notNull cards) do
          chooseTargetM iid cards \card ->
            chooseTargetM iid insects (`placeCardAsSwarm` card)

drawTopHexFromDiscard :: ReverseQueue m => ScenarioAttrs -> InvestigatorId -> m ()
drawTopHexFromDiscard attrs iid = do
  for_ (find (`cardMatch` CardWithTrait Hex) attrs.discard) \card -> do
    obtainCard card
    push $ InvestigatorDrewEncounterCard iid card

resolutionFor :: Faction -> Resolution
resolutionFor = \case
  RedFaction -> Resolution 5
  BlueFaction -> Resolution 6
  GreenFaction -> Resolution 7

instance RunMessage WarOfTheOuterGods where
  runMessage msg s@(WarOfTheOuterGods attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup WarOfTheOuterGods attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherFactionSets"
        li "agendaDecks"
        li.nested "placeLocations" do
          li "beginAtArkham"
        li "spawnCultists"
        li "setAsideAssets"
        li "shuffleRemainder"
        li.validate False "epicMultiplayer"

      additionalRules "factions"
      additionalRules "factionAgendas"
      additionalRules "inTheLead"
      additionalRules "warring"
      additionalRules "wards"
      additionalRules "placeAroundThisLocation"
      additionalRules "swarmingX"

      gather Set.WarOfTheOuterGods
      gatherAndSetAside Set.DeathOfStars
      gatherAndSetAside Set.ChildrenOfParadise
      gatherAndSetAside Set.SwarmOfAssimilation

      setAgendaDeckN
        1
        [ Agendas.theIncubationOfTheEgg
        , Agendas.theIncubationProgresses
        , Agendas.theIncubationNearsCompletion
        ]
      setAgendaDeckN
        2
        [ Agendas.theSummoningOfSilenus
        , Agendas.theSummoningProgresses
        , Agendas.theSummoningNearsCompletion
        ]
      setAgendaDeckN
        3
        [ Agendas.theProliferationOfTheSwarm
        , Agendas.theProliferationProgresses
        , Agendas.theProliferationNearsCompletion
        ]
      setActDeck [Acts.warOfTheOuterGods, Acts.closeThePortal, Acts.closeAllPortals]

      setAside [Locations.hubDimension]

      startAt =<< place Locations.arkham
      athenaeum <- place Locations.athenaeumOfTheEmptySky
      shrine <- place Locations.shrineOfMaghanArkat
      burningPit <- place Locations.theBurningPit
      placeAll
        [ Locations.streetsOfProvidence
        , Locations.theArcade
        , Locations.streetsOfMontreal
        , Locations.chateauRamezay
        , Locations.streetsOfNewYorkCity
        , Locations.thePenthouse
        ]

      enemyAt_ Enemies.discipleOfTheSwarm burningPit
      enemyAt_ Enemies.nihilisticStargazer athenaeum
      enemyAt_ Enemies.zealotOfParadise shrine

      setAside
        [ Assets.cloakOfTheOuterRealm
        , Assets.pocketPortal
        , Assets.dreadedEnd
        , Assets.bladeOfArkat
        , Assets.enchantedSkull
        ]
    PlaceDoomOnAgenda n canAdvance -> do
      -- During the "place doom on the current agenda" step, place 1 doom on
      -- each agenda, in faction order.
      for_ factionOrder \f ->
        selectForMaybeM (factionAgenda f) \agenda -> placeDoom attrs agenda n
      pushWhen (canAdvance == CanAdvance) AdvanceAgendaIfThresholdSatisfied
      pure s
    AdvanceAgendaIfThresholdSatisfied -> do
      -- Check the doom threshold for each agenda one at a time, in faction
      -- order. Once the war is over only a single agenda remains and the
      -- default behavior applies.
      stage4 <- selectAny $ AgendaWithStep 4
      if stage4
        then WarOfTheOuterGods <$> liftRunMessage msg attrs
        else do
          for_ factionOrder \f ->
            selectForMaybeM (factionAgenda f) (`forTarget` msg)
          pure s
    HuntersMove -> do
      -- During the "hunter enemies move" step, each ready, unengaged enemy
      -- with the warring keyword moves once toward the nearest warring enemy
      -- of a different faction. These moves are batched with hunter moves so
      -- the players choose the order.
      movers <- getWarringMovers
      for_ movers \enemy ->
        push
          $ HandleGroupTarget HunterGroup (toTarget enemy) [ScenarioSpecific "warringMove" (toJSON enemy)]
      WarOfTheOuterGods <$> liftRunMessage msg attrs
    EnemiesAttack -> do
      -- During the "resolve enemy attacks" step, each unengaged enemy with
      -- the warring keyword attacks a warring enemy at its location of a
      -- different faction. These attacks are batched with regular enemy
      -- attacks so the players choose the order.
      attackers <- getWarringAttackers
      unless (null attackers) do
        push $ EnemyAttacks [ScenarioSpecific "warringAttack" (toJSON enemy) | enemy <- attackers]
      WarOfTheOuterGods <$> liftRunMessage msg attrs
    ScenarioSpecific "warringMove" v -> do
      let enemy = toResult v
      whenM (enemy <=~> (warringEnemy <> ReadyEnemy <> UnengagedEnemy)) do
        getEnemyFaction enemy >>= traverse_ \f -> do
          colocated <-
            select $ EnemyAt (locationWithEnemy enemy) <> mapOneOf factionEnemy (filter (/= f) factionOrder)
          when (null colocated) do
            field EnemyLocation enemy >>= traverse_ \loc -> do
              nearest <- select $ NearestEnemyToLocation loc (warringEnemy <> not_ (factionEnemy f))
              unless (null nearest) do
                -- Every choice (which nearest enemy to move toward, then which
                -- first step along the shortest path) ultimately just moves the
                -- enemy to a connecting location. If they all collapse to a
                -- single destination, the choice is meaningless, so move there
                -- automatically rather than prompting.
                destinations <- nub . concat <$> for nearest \target ->
                  field EnemyLocation target >>= \case
                    Nothing -> pure []
                    Just targetLoc -> select $ ClosestPathLocation loc targetLoc
                case destinations of
                  [dest] -> push $ EnemyMove enemy dest
                  _ -> do
                    lead <- getLead
                    chooseOrRunOneM lead do
                      questionSourced enemy
                      targets nearest \target ->
                        withLocationOf target \targetLoc -> do
                          nextSteps <- select $ ClosestPathLocation loc targetLoc
                          chooseOrRunOneM lead do
                            questionSourced enemy
                            targets nextSteps $ push . EnemyMove enemy
      pure s
    ScenarioSpecific "warringAttack" v -> do
      let enemy = toResult v
      whenM (enemy <=~> (warringEnemy <> ReadyEnemy <> UnengagedEnemy)) do
        warringTargets <- getWarringTargets enemy
        unless (null warringTargets) do
          push $ ScenarioSpecific "warringAttackWindow" v
          push $ ScenarioSpecific "warringAttackResolve" v
      pure s
    ScenarioSpecific "warringAttackResolve" v -> do
      let enemy = toResult v
      warringTargets <- getWarringTargets enemy
      unless (null warringTargets) do
        damage <- field EnemyHealthDamage enemy
        horror <- field EnemySanityDamage enemy
        mods <- getModifiers enemy
        let total =
              if AttackDealsEitherDamageOrHorror `elem` mods
                then max damage horror
                else damage + horror
        withHealth <- forMaybeM warringTargets \target ->
          fmap (target,) <$> field EnemyRemainingHealth target
        unless (null withHealth) do
          let least = minimumEx $ map snd withHealth
          let weakest = [target | (target, health) <- withHealth, health == least]
          lead <- getLead
          chooseOrRunOneM lead $ targets weakest \target -> do
            isSwarmCard <- target <=~> IsSwarm
            nonAttackEnemyDamage Nothing enemy total target
            -- If an enemy attacks and defeats another enemy (other than a
            -- swarm card), place 1 doom on the agenda matching the attacking
            -- enemy's faction.
            when (total >= least && not isSwarmCard) do
              getEnemyFaction enemy >>= traverse_ \f ->
                placeDoomOnFactionAgenda attrs f 1
      pure s
    ScenarioSpecific "excessDoom" v -> do
      let (deckN, excess) = toResult v :: (Int, Int)
      magenda <- selectOne (AgendaWithDeckId deckN)
      magenda' <- maybe (selectOne AnyAgenda) (pure . Just) magenda
      for_ magenda' \agenda -> placeDoom attrs agenda excess
      pure s
    ResolveChaosToken _ Cultist _ | isHardExpert attrs -> do
      selectEach (EnemyWithTrait Mutated) \enemy -> placeMutations attrs enemy 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      drawTopHexFromDiscard attrs iid
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      commitSwarmCards iid True
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case token.face of
        Cultist -> do
          nearest <- select $ NearestEnemyTo iid $ EnemyWithTrait Mutated
          unless (null nearest) do
            chooseOrRunOneM iid $ targets nearest \enemy -> placeMutations attrs enemy 1
        Tablet -> drawTopHexFromDiscard attrs iid
        ElderThing -> commitSwarmCards iid False
        _ -> pure ()
      pure s
    ScenarioResolution res -> scope "resolutions" do
      investigators <- allInvestigators
      let
        addStoryAssetChoice def = addCampaignCardToDeckChoice investigators DoNotShuffleIn def
        killedAndLost key = do
          resolution key
          eachInvestigator (kill attrs)
          gameOver
        factionKey = \case
          GreenFaction -> "greenFactionInTheLead"
          BlueFaction -> "blueFactionInTheLead"
          RedFaction -> "redFactionInTheLead"
      case res of
        NoResolution -> do
          resolution "noResolution"
          leads <- getLeadFactions
          case leads of
            [f] -> push $ ScenarioResolution (resolutionFor f)
            fs -> do
              leadChooseOneM $ for_ fs \f ->
                labeled' (factionKey f) $ push $ ScenarioResolution (resolutionFor f)
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
          addStoryAssetChoice Assets.cloakOfTheOuterRealm
          addStoryAssetChoice Assets.pocketPortal
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          addStoryAssetChoice Assets.cloakOfTheOuterRealm
          addStoryAssetChoice Assets.enchantedSkull
          endOfScenario
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          addStoryAssetChoice Assets.cloakOfTheOuterRealm
          addStoryAssetChoice Assets.dreadedEnd
          endOfScenario
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXp' attrs
          addStoryAssetChoice Assets.cloakOfTheOuterRealm
          addStoryAssetChoice Assets.bladeOfArkat
          endOfScenario
        Resolution 5 -> killedAndLost "resolution5"
        Resolution 6 -> killedAndLost "resolution6"
        Resolution 7 -> killedAndLost "resolution7"
        _ -> error "Unknown resolution"
      pure s
    _ -> WarOfTheOuterGods <$> liftRunMessage msg attrs
