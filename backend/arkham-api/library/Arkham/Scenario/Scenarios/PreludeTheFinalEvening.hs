module Arkham.Scenario.Scenarios.PreludeTheFinalEvening (preludeTheFinalEvening) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Calculation
import Arkham.Campaigns.TheFeastOfHemlockVale.CampaignSteps qualified as Steps
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Query (
  getInvestigators,
  getJustLocationByName,
  getPlayerCount,
  getSetAsideCardMaybe,
 )
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Id (InvestigatorId)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern PassedThisSkillTest)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (crossOut, record, remember, remembered)
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Resident))

newtype PreludeTheFinalEvening = PreludeTheFinalEvening ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeTheFinalEvening :: Difficulty -> PreludeTheFinalEvening
preludeTheFinalEvening difficulty =
  scenarioWith
    PreludeTheFinalEvening
    "10679b"
    "The Vale"
    difficulty
    [ ".     triangle square"
    , "moon  triangle square"
    , "moon  diamond  star"
    , "heart diamond  star"
    , "heart circle   spade"
    , ".     circle   spade"
    ]
    $ (hasEncounterDeckL .~ False)
    . (referenceL .~ "10704")

instance HasChaosTokenValue PreludeTheFinalEvening where
  getChaosTokenValue iid tokenFace (PreludeTheFinalEvening attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | The enemy-side card def for each resident. Residents are double-sided in
the physical game; in the engine the asset side and enemy side are separate
cards, so flipping a resident means swapping one for the other.
-}
residentEnemyDef :: Resident -> CardDef
residentEnemyDef = \case
  WilliamHemlock -> Enemies.williamHemlock
  RiverHawthorne -> Enemies.riverHawthorne
  MotherRachel -> Enemies.motherRachelStarbornHerald
  SimeonAtwood -> Enemies.simeonAtwood
  LeahAtwood -> Enemies.leahAtwood
  TheoPeters -> Enemies.theoPeters
  GideonMizrah -> Enemies.gideonMizrah
  JudithPark -> Enemies.judithPark

instance RunMessage PreludeTheFinalEvening where
  runMessage msg s@(PreludeTheFinalEvening attrs) = runQueueT $ campaignI18n $ scope "prelude4" $ case msg of
    PreScenarioSetup -> scope "intro" do
      hasPlan <- getHasRecord DrMarquezHasAPlan
      flavor do
        h "title"
        p "intro1"
        ul do
          li.validate hasPlan "proceedToIntro2"
          li.validate (not hasPlan) "skipToIntro5"
      if hasPlan
        then do
          motherRachelRel <- getRelationshipLevel MotherRachel
          flavor do
            h "title"
            p "intro2"
            ul do
              li.validate (motherRachelRel >= 3) "doneNothingWrongTruth"
              li.validate (motherRachelRel >= 3) "doneNothingWrongLie"
              li "sentencedToDeath"
          leadChooseOneM do
            when (motherRachelRel >= 3) do
              labeled' "doneNothingWrongTruth" intro3
              labeled' "doneNothingWrongLie" intro4
            labeled' "sentencedToDeath" intro5
        else intro5
      pure s
    Setup -> runScenarioSetup PreludeTheFinalEvening attrs do
      setup $ ul do
        li "gatherSets"
        li "dayThree"
        li "buildActAgenda"
        li.nested "placeLocations" do
          li "crossroadsAndOldMill"
          li "frenziedRevelers"
        li.nested "residents" do
          li "removeResidents"
          li "placeResidents"
          li "setOutOfPlay"
        li "startAt"
        li "placeDoom"
        unscoped $ li "readyToBegin"

      gather Set.TheFinalDay
      gather Set.DayOfTheFeast
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      -- "Around the Table" serves as both the current act and the current
      -- agenda. We model it as the agenda (the doom track that leads to "The
      -- Sleep"); the act objective is added with the full Final Evening
      -- scenario. The special agenda "Lambs to the Slaughter" is set aside.
      setAgendaDeck [Agendas.aroundTheTable]
      setAside [Agendas.lambsToTheSlaughter]

      boardingHouse <- place Locations.boardingHouseDay
      theCrossroads <- place Locations.theCrossroadsEvening
      hemlockChapel <- place Locations.hemlockChapelDay
      placeAll [Locations.theAtwoodHouseDay]
      tadsStore <- place Locations.tadsGeneralStoreDay
      valeSchoolhouse <- place Locations.valeSchoolhouseDay
      theCommons <- place Locations.theCommonsDay
      theOldMill <- place Locations.theOldMillEvening

      n <- getPlayerCount
      createEnemyAt_ Enemies.frenziedReveler theCommons
      when (n >= 3) $ createEnemyAt_ Enemies.frenziedReveler hemlockChapel

      interrupted <- getHasRecord TheInvestigatorsInterruptedTheFeast
      if interrupted then startAt boardingHouse else startAt theCrossroads

      -- If North Point Mine was never surveyed, Leah Atwood's name is crossed out.
      northPointSurveyed <- getHasRecord (AreasSurveyed NorthPointMine)
      unless northPointSurveyed $ record LeahCrossedOut

      crossed0 <- getCrossedOutResidents
      let crossed =
            if northPointSurveyed || LeahAtwood `elem` crossed0
              then crossed0
              else LeahAtwood : crossed0

      -- Remove each crossed-out resident from the game.
      for_ crossed (obtainCard <=< fetchCard)

      -- Place each remaining resident. If their Relationship Level is at or below
      -- the listed threshold, they are placed on their enemy side instead. The
      -- enemy and asset are the two sides of the same double-sided card, so when
      -- we place the enemy we must remove the set-aside asset version (the enemy
      -- card code does not match the asset's, so creating the enemy otherwise
      -- leaves the asset behind in set aside).
      let placeResident resident loc threshold enemyCard assetCard =
            unless (resident `elem` crossed) do
              rel <- getRelationshipLevel resident
              if rel <= threshold
                then do
                  obtainCard assetCard
                  createEnemyAt_ enemyCard loc
                else createAssetAt_ assetCard (AtLocation loc)

      unless (MotherRachel `elem` crossed)
        $ createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation theCrossroads)

      bertieEpiphany <- getHasRecord BertieHadAnEpiphany
      when bertieEpiphany do
        obtainCard Assets.bertieMusgraveATrueAesthete
        createEnemyAt_ Enemies.bertieMusgrave theCrossroads

      -- If the investigators interrupted the Feast, Dr. Marquez joins the fray.
      -- TODO: search each investigator's deck and all out-of-play areas for Dr.
      -- Marquez, then put her into play under an investigator's control (she
      -- does not take up an ally slot for the remainder of this prelude).

      placeResident LeahAtwood hemlockChapel 2 Enemies.leahAtwood Assets.leahAtwoodTheValeCook
      placeResident
        SimeonAtwood
        valeSchoolhouse
        3
        Enemies.simeonAtwood
        Assets.simeonAtwoodDedicatedTroublemaker
      placeResident GideonMizrah theCommons 3 Enemies.gideonMizrah Assets.gideonMizrahSeasonedSailor
      placeResident JudithPark theOldMill 2 Enemies.judithPark Assets.judithParkTheMuscle
      placeResident TheoPeters tadsStore 2 Enemies.theoPeters Assets.theoPetersJackOfAllTrades

      placeDoomOnAgenda n
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          believed <- getHasRecord TheInvestigatorsBelieved
          push $ if believed then R2 else R4
        Resolution 1 -> do
          resolution "resolution1"
          push R4
        Resolution 2 -> do
          resolution "resolution2"
          push R3
        Resolution 3 -> do
          resolutionWithChooseOne "resolution3" do
            labeled' "replay" do
              crossOut TheInvestigatorsBelieved
              endOfScenarioThen Steps.PreludeTheFinalEvening
            labeled' "loseTheCampaign" do
              eachInvestigator (kill attrs)
              gameOver
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXp' attrs
          endOfScenarioThen Steps.FateOfTheVale
        _ -> error "invalid resolution"
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let drawOrResource cnt = chooseOneM iid do
            labeled' ("draw" <> tshow cnt) $ drawCards iid source cnt
            labeled' ("gainResource" <> tshow cnt) $ gainResources iid source cnt
      let takeControlOfResident :: ReverseQueue m => Resident -> m ()
          takeControlOfResident resident =
            selectOne (assetIs (toCardDef resident)) >>= traverse_ (takeControlOfAsset iid)
      -- Defined at the @codex@ scope so the XP key always resolves to
      -- @codex.xp.<key>@ regardless of the sub-scope it is invoked from.
      let
        awardXp :: ReverseQueue m => Text -> Int -> m ()
        awardXp xpKey xpAmount =
          eachInvestigator \i -> gainXp i attrs (ikey ("xp." <> xpKey)) xpAmount
      let recruitResident resident xpKey relIncrease xpAmount = do
            increaseRelationshipLevel resident relIncrease
            awardXp xpKey xpAmount
            takeControlOfResident resident
      case n of
        1 -> scope "motherRachel" do
          believed <- getHasRecord TheInvestigatorsBelieved
          lied <- getHasRecord TheInvestigatorsLiedToMotherRachel
          if
            | believed -> do
                flavor $ setTitle "title" >> p.green "believed"
                -- Flip each resident except Mother Rachel to their enemy side.
                traverse_
                  flipResidentToEnemy
                  [LeahAtwood, SimeonAtwood, GideonMizrah, JudithPark, TheoPeters]
                -- Gather each set-aside resident and put each into play at an
                -- empty location, enemy side faceup.
                for_ [WilliamHemlock, RiverHawthorne] \resident ->
                  getSetAsideCardMaybe (toCardDef resident) >>= traverse_ \card -> do
                    obtainCard card
                    locs <- select EmptyLocation
                    unless (null locs) $ chooseTargetM iid locs $ createEnemyAt_ (residentEnemyDef resident)
            -- TODO: remove all doom from the current agenda and swap it with
            -- the set-aside Lambs to the Slaughter agenda. This transitions
            -- into the full Feast of Hemlock Vale scenario, which is not yet
            -- implemented, so the agenda swap is deferred to that work.
            | lied -> do
                flavor $ setTitle "title" >> p.green "lied"
                flipResidentToEnemy MotherRachel >>= traverse_ \eid ->
                  nonAttackEnemyDamage (Just iid) source 2 eid
            | otherwise -> do
                flavor $ setTitle "title" >> p.green "otherwise"
                void $ flipResidentToEnemy MotherRachel
        2 -> scope "leahAtwood" do
          sawMine <- getHasRecord LeahSawSomethingInTheMine
          if sawMine
            then storyWithChooseOneM' (setTitle "title" >> p.green "leah1") do
              labeled' "askMine" do
                flavor $ setTitle "title" >> p.green "leah2"
                setRelationshipLevel LeahAtwood 0
              labeled' "sayNothing" do
                flavor $ setTitle "title" >> p.green "leah3"
                recruitResident LeahAtwood "leahAtwood" 1 1
            else do
              flavor $ setTitle "title" >> p.green "otherwise"
              increaseRelationshipLevel LeahAtwood 1
              awardXp "leahAtwood" 1
        3 -> scope "simeonAtwood" do
          fireworks <- getHasRecord TheValeIsFullOfFireworks
          if fireworks
            then do
              flavor $ setTitle "title" >> p.green "fireworks"
              recruitResident SimeonAtwood "simeonAtwood" 2 2
            else do
              flavor $ setTitle "title" >> p.green "otherwise"
              increaseRelationshipLevel SimeonAtwood 1
              awardXp "simeonAtwood" 1
        4 -> scope "williamHemlock" do
          flavor $ setTitle "title" >> p.green "body"
          recruitResident WilliamHemlock "williamHemlock" 1 2
          setRelationshipLevel RiverHawthorne 0
          theAtwoodHouse <- getJustLocationByName "The Atwood House"
          getSetAsideCardMaybe (toCardDef RiverHawthorne) >>= traverse_ obtainCard
          createEnemyAt_ Enemies.riverHawthorne theAtwoodHouse
        5 -> scope "riverHawthorne" do
          flavor $ setTitle "title" >> p.green "body"
          recruitResident RiverHawthorne "riverHawthorne" 1 2
          setRelationshipLevel WilliamHemlock 0
          theAtwoodHouse <- getJustLocationByName "The Atwood House"
          getSetAsideCardMaybe (toCardDef WilliamHemlock) >>= traverse_ obtainCard
          createEnemyAt_ Enemies.williamHemlock theAtwoodHouse
        6 -> scope "gideonMizrah" do
          toldTale <- getHasRecord GideonToldTheTaleOfTheAnnabelleLee
          foundTreasure <- getHasRecord GideonFoundHisTreasure
          if toldTale && foundTreasure
            then do
              flavor $ setTitle "title" >> p.green "toldTale"
              record GideonFinishedTheTaleOfAnnabelleLee
              recruitResident GideonMizrah "gideonMizrah" 1 2
            else do
              flavor $ setTitle "title" >> p.green "otherwise"
              increaseRelationshipLevel GideonMizrah 1
              awardXp "gideonMizrah" 1
        7 -> scope "judithPark" do
          backedUp <- getHasRecord YouBackedJudithUp
          if backedUp
            then do
              flavor $ setTitle "title" >> p.green "backedUp"
              weapons <- select $ inHandOf NotForPlay iid <> basic (#asset <> #weapon)
              unless (null weapons) $ chooseOneM iid do
                unscoped skip_
                targets weapons $ putCardIntoPlay iid
              recruitResident JudithPark "judithPark" 1 2
            else do
              flavor $ setTitle "title" >> p.green "otherwise"
              increaseRelationshipLevel JudithPark 1
              awardXp "judithPark" 1
        8 -> scope "theoPeters" do
          secondThoughts <- getHasRecord TheoIsHavingSecondThoughts
          reconciled <- getHasRecord TheoReconciledWithHelen
          if
            | secondThoughts && reconciled -> do
                flavor $ setTitle "title" >> p.green "both"
                recruitResident TheoPeters "theoPeters" 1 2
            | secondThoughts -> do
                flavor $ setTitle "title" >> p.green "secondThoughts"
                recruitResident TheoPeters "theoPeters" 1 1
            | otherwise -> do
                flavor $ setTitle "title" >> p.green "otherwise"
                takeControlOfResident TheoPeters
        9 -> scope "boardingHouse" do
          flavor $ setTitle "title" >> p.green "body"
          drawOrResource 3
        10 -> scope "theCrossroads" do
          flavor $ setTitle "title" >> p.green "body"
          theCrossroads <- getJustLocationByName "The Crossroads"
          enemies <- select AnyEnemy
          unless (null enemies) $ chooseTargetM iid enemies \enemy -> do
            disengageFromAll enemy
            enemyMoveTo attrs enemy theCrossroads
            exhaustEnemy attrs enemy
        11 -> scope "hemlockChapel" do
          flavor $ setTitle "title" >> p.green "body"
          drawOrResource 1
        12 -> scope "theOldMill" do
          flavor $ setTitle "title" >> p.green "body"
          theOldMill <- getJustLocationByName "The Old Mill"
          enemies <-
            select
              $ EnemyAt (oneOf [LocationWithId theOldMill, connectedTo (LocationWithId theOldMill)])
              <> withTrait Resident
          unless (null enemies) $ chooseTargetM iid enemies \enemy -> do
            sid <- getRandom
            chooseBeginSkillTest sid iid attrs enemy [#willpower, #intellect, #combat, #agility] (Fixed 2)
        13 -> scope "theAtwoodHouse" do
          hashingItOut <- remembered TheHemlocksAreHashingItOut
          william <- getSetAsideCardMaybe (toCardDef WilliamHemlock)
          river <- getSetAsideCardMaybe (toCardDef RiverHawthorne)
          theAtwoodHouse <- getJustLocationByName "The Atwood House"
          riverLegacy <- getHasRecord RiverIsReclaimingTheirLegacy
          williamResolved <- getHasRecord WilliamIsResolved

          let
            body :: HasI18n => FlavorTextBuilder ()
            body = do
              setTitle "title"
              compose.green do
                p.validate hashingItOut "hashingItOut"
                hr
                compose.validate (not hashingItOut) do
                  p "otherwise"
                  ul do
                    if hashingItOut
                      then do
                        li "sideRiver"
                        li "sideWilliam"
                      else do
                        li.validate (isJust river && riverLegacy) "sideRiver"
                        li.validate (isJust william && williamResolved) "sideWilliam"
                    li "letFight"

          if hashingItOut
            then flavor body
            else storyWithChooseOneM' body do
              labeledValidate' (isJust river && riverLegacy) "sideRiver" do
                createAssetAt_ Assets.riverHawthorneBigInNewYork (AtLocation theAtwoodHouse)
              labeledValidate' (isJust william && williamResolved) "sideWilliam" do
                createAssetAt_ Assets.williamHemlockAspiringPoet (AtLocation theAtwoodHouse)
              labeled' "letFight" do
                when (williamResolved || riverLegacy) $ remember TheHemlocksAreHashingItOut

          when hashingItOut do
            record TheHemlocksMadeATruce
            increaseRelationshipLevel WilliamHemlock 1
            increaseRelationshipLevel RiverHawthorne 1
            awardXp "theAtwoodHouse" 2
            chooseOneM iid do
              for_ william $ labeled' "william" . takeControlOfSetAsideAsset iid
              for_ river $ labeled' "river" . takeControlOfSetAsideAsset iid
        14 -> scope "tadsGeneralStore" do
          flavor $ setTitle "title" >> p.green "body"
          gainResources iid source 3
        15 -> scope "valeSchoolhouse" do
          flavor $ setTitle "title" >> p.green "body"
          valeSchoolhouse <- getJustLocationByName "Vale Schoolhouse"
          enemies <- select $ EnemyAt (LocationWithId valeSchoolhouse) <> withTrait Resident
          unless (null enemies) $ chooseTargetM iid enemies \enemy -> do
            sid <- getRandom
            chooseBeginSkillTest sid iid attrs enemy [#willpower, #intellect, #combat, #agility] (Fixed 2)
        16 -> scope "theCommons" do
          flavor $ setTitle "title" >> p.green "body"
          locations <- select $ not_ (locationWithInvestigator iid)
          chooseOneM iid do
            unscoped skip_
            targets locations \loc -> do
              moveTo attrs iid loc
              gainActions iid attrs 1
        100 -> scope "drRosaMarquez" do
          flavor $ setTitle "title" >> p.green "body"
          investigators <- getInvestigators
          chooseOneM iid $ for_ investigators \i -> targeting i $ drawCards i source 5
        101 -> scope "bertieMusgrave" do
          flavor $ setTitle "title" >> p.green "body"
          chooseAndDiscardCards iid source 3
          record BertiePerished
        _ -> error "invalid codex entry"
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      -- Codex 12/15: a successful "test any skill (2)" automatically evades the
      -- chosen Resident enemy (carried as the skill test's target).
      getSkillTestTargetedEnemy >>= traverse_ (automaticallyEvadeEnemy iid)
      pure s
    _ -> PreludeTheFinalEvening <$> liftRunMessage msg attrs
   where
    intro3 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro3"
      addChaosToken Cultist
      eachInvestigator \iid -> do
        cards <-
          select
            $ oneOf [inDeckOf iid, inHandOf NotForPlay iid, inDiscardOf iid]
            <> basic (cardIs Assets.drRosaMarquezBestInHerField)
        for_ cards (push . Msg.RemovePlayerCardFromGame True)
      record TheInvestigatorsBelieved
    intro4 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro4"
      addChaosToken Cultist
      addChaosToken ElderThing
      record TheInvestigatorsLiedToMotherRachel
    intro5 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro5"
      addChaosToken Tablet
      addChaosToken ElderThing
      record TheInvestigatorsInterruptedTheFeast
    -- \| Flip a resident from their asset side to their enemy side, in place,
    -- returning the created enemy (or Nothing if they are not in play as an
    -- asset). Used by Codex 1.
    flipResidentToEnemy resident =
      selectOne (assetIs (toCardDef resident)) >>= \case
        Nothing -> pure Nothing
        Just aid -> do
          mloc <- field AssetLocation aid
          case mloc of
            Nothing -> pure Nothing
            Just loc -> do
              removeFromGame aid
              Just <$> createEnemyAt (residentEnemyDef resident) loc
