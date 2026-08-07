{- | The Dream-Eaters achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign id
("06"), so earns stay unconditional here.

The Dream-Eaters prints TWO achievement lists, one per mini-campaign. Both are
earnable in campaign "06" — you cannot play The Dream-Quest without playing
campaign "06" — so the split is purely a display grouping
('achievementCampaignPart'); detection here is scoped by scenario instead.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'). That store rides on 'CampaignAttrs', which
The Dream-Eaters swaps between the two halves when the full campaign switches
sides — which is exactly what we want, since every tracker below belongs to one
half's scenarios.

Campaign-completion detections key on the terminal 'Record's each final
resolution writes, NOT on 'ScenarioResolution' (the Scenario wrapper clearQueues
twice while processing one, wiping even Priority pushes) and NOT on
'EpilogueStep' (a partial-mode Where the Gods Dwell pushes GameOver from the
resolution itself, so the epilogue step is not a reliable landing point).
-}
module Arkham.Campaign.Campaigns.TheDreamEaters.Achievements (
  runTheDreamEatersAchievements,
) where

import Arkham.Achievement
import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Card
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCard, EnemyPlacement, EnemyRemainingHealth))
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (getCampaignMeta, stored)
import Arkham.Helpers.Log (getHasRecord, hasRecord, scenarioCount)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorTokens))
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Location
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Tracing
import Arkham.Trait (Trait (Ghoul, Spider, Zoog))
import Arkham.Treachery.Types qualified as Treachery
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runTheDreamEatersAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runTheDreamEatersAchievements msg = whenEligibleCampaign $ case msg of
  -- Per-game trackers reset as their scenario is set up, so "during a single
  -- game" stays true even if a scenario is somehow revisited.
  Setup -> do
    whenScenarioIs beyondTheGatesOfSleepId $ setStore zoogDefeatedKey False
    whenScenarioIs wakingNightmareId $ setStore doctorHurtKey False
    whenScenarioIs aThousandShapesOfHorrorId $ setStore dejaVuAbilitiesKey ([] :: [Text])
    whenScenarioIs pointOfNoReturnId $ setStore flippedLocationsKey ([] :: [CardCode])
    whenScenarioIs whereTheGodsDwellId $ setStore hiddenCardsGivenKey ([] :: [Text])
    -- "Master of Unlocking" is per-scenario and The Silver Key travels between
    -- both halves, so its tally resets on every setup rather than a specific one.
    setStore silverKeyHorrorKey (0 :: Int)

  -- Enemy defeats. The campaign sees Defeated before the enemy processes it, so
  -- the entity is still in play and queryable. 'Defeated' already carries the
  -- defeated card's traits, which is what the trait-based tallies read.
  Defeated (EnemyTarget eid) _ source traits -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    -- "Aww, But They're So Cute": bookkeeping only — any Zoog defeated in Beyond
    -- the Gates of Sleep disqualifies the earn at the end of the scenario.
    when (Zoog `elem` traits) $ whenScenarioIs beyondTheGatesOfSleepId do
      setStore zoogDefeatedKey True

    -- "Everyone's a Feminist Until There Is a Spider Around": twenty Spider
    -- enemies across the campaign. Swarm cards explicitly do not count.
    when (Spider `elem` traits) do
      placement <- field EnemyPlacement eid
      unless placement.isSwarm do
        n <- storedInt spidersDefeatedKey
        setStore spidersDefeatedKey (n + 1)
        when (n + 1 >= 20) $ earnWeb EveryonesAFeministUntilThereIsASpiderAround

    -- "Moon Lizards? I Don't Believe They Exist".
    when (cardDef == Enemies.moonLizard) $ whenScenarioIs darkSideOfTheMoonId do
      earnDreamQuest MoonLizardsIDontBelieveTheyExist

    -- "Barkham Horror Enthusiast": the killing source has to unwrap to a cat or
    -- dog ally. Those are not printed traits, so the assets carry a "cat"/"dog"
    -- 'cdTags' entry instead; a fight ability's damage is sourced from the asset's
    -- ability, which the HasField accessor sees through.
    when (cardDef == Enemies.catsFromSaturn) do
      for_ source.asset \aid -> do
        assetDef <- fieldMap Asset.AssetCard toCardDef aid
        when (any (`elem` cdTags assetDef) ["cat", "dog"]) do
          earnDreamQuest BarkhamHorrorEnthusiast

    -- "This Isn't Even My Final Form!": the True Shape only exists once Truth and
    -- Lies has exposed it, so defeating it implies exposing it.
    when (cardDef == Enemies.nyarlathotepTrueShape) $ earnDreamQuest ThisIsntEvenMyFinalForm

    -- "The Ishimura Flex": all four Legs of Atlach-Nacha in a single round.
    when (cardDef `elem` legsOfAtlachNacha) do
      defeated <- storedList legsDefeatedThisRoundKey
      let defeated' = nub (toCardCode cardDef : defeated)
      setStore legsDefeatedThisRoundKey defeated'
      when (length defeated' >= length legsOfAtlachNacha) $ earnWeb TheIshimuraFlex

  -- "Give Them Something To Talk About": every hidden form of Nyarlathotep added
  -- to the victory display in a single round. Beyond Dreams shuffles exactly
  -- 1 + player count of the five forms into the encounter deck and removes the
  -- rest from the game, so that is the number to reach.
  AddToVictory _ (EnemyTarget eid) -> whenScenarioIs whereTheGodsDwellId do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (cardDef `elem` nyarlathotepForms) do
      forms <- storedList nyarlathotepsThisRoundKey
      let forms' = nub (toCardCode cardDef : forms)
      setStore nyarlathotepsThisRoundKey forms'
      n <- getPlayerCount
      when (length forms' >= n + 1) $ earnDreamQuest GiveThemSomethingToTalkAbout

  -- Round-scoped tallies. Reset on both ends of the boundary so a tally can never
  -- straddle two rounds.
  BeginRound -> resetRoundTallies
  EndRound -> resetRoundTallies
  -- "You Spin Me Right 'Round" is phase-scoped rather than round-scoped.
  EndPhase -> setStore atlachNachaSpinKey (0 :: Int)
  -- "Losing My Religion": all 10 Signs of the Gods in one playthrough of The
  -- Search for Kadath. The scenario count is updated by the very message we are
  -- dispatching on, so the increment has to be added to what we read back.
  ScenarioCountIncrementBy key n | key == SignOfTheGods -> do
    whenScenarioIs theSearchForKadathId do
      current <- scenarioCount SignOfTheGods
      when (current + n >= 10) $ earnDreamQuest LosingMyReligion

  -- "Fantasy Flight Games (R) Does Not Condone Accomplishing This Achievement":
  -- the Cats of Ulthar's forced ability is the only thing that writes this.
  RecordForInvestigator _ key | key == toCampaignLogKey HasBrokenTheLawOfUlthar -> do
    whenScenarioIs theSearchForKadathId
      $ earnDreamQuest FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement

  -- "I Remember This Place": Another Path (the flip side of Tower of Koth) is the
  -- only way out of the Underworld.
  Record key | key == toCampaignLogKey TheInvestigatorsFoundAWayOutOfTheUnderworld -> do
    earnWeb IRememberThisPlace

  {- Campaign completions. Where the Gods Dwell resolutions 3-5 are only reachable
  from resolutions 1 and 2, and Weaver of the Cosmos resolutions 3-5 only from its
  resolution 1, so each of these six records means "that half was won". The losing
  endings (Nyarlathotep's invasion / the bridge completed) never write them. -}
  Record key | key `elem` map toCampaignLogKey dreamQuestEndings -> do
    -- "Beware The Black Cat" only asks that the campaign be COMPLETED, so it hangs
    -- off every Dream-Quest ending rather than the winning ones.
    whenM (getHasRecord OkayFineHaveItYourWayThen) $ earnDreamQuest BewareTheBlackCat
    when (key `elem` map toCampaignLogKey dreamQuestWins) do
      whenM ((>= 3) <$> activeUltimatumCount) $ earnDreamQuest DreamQuestLineInTheSand
      whenM ((== Just Expert) <$> currentDifficulty) $ earnDreamQuest DreamlandsExpertise
  Record key | key `elem` map toCampaignLogKey webOfDreamsWins -> do
    whenM ((>= 3) <$> activeUltimatumCount) $ earnWeb WebOfDreamsLineInTheSand
    whenM ((== Just Expert) <$> currentDifficulty) $ earnWeb UnderworldExpertise

  {- "Reunited and it Feels So Good": the full 8-part campaign only, and only the
  three epilogues that both reunite the groups AND win the campaign — epilogue 6
  (awoke + returned to reality), 12 (stayed in the Dreamlands + still in the
  Dreamlands) and 15 (traveled beneath the monastery + never escaped). Epilogue 2
  also reunites them, in the waking world, but is not a win. -}
  CampaignStep EpilogueStep -> do
    meta <- getCampaignMeta @Metadata
    when (meta.mode == FullMode) do
      awoke <- hasRecordIn TheDreamQuest TheDreamersAwoke
      stayed <- hasRecordIn TheDreamQuest TheDreamersStayedInTheDreamlandsForever
      traveled <- hasRecordIn TheDreamQuest TheDreamersTraveledBeneathTheMonastery
      returned <- hasRecordIn TheWebOfDreams TheInvestigatorsReturnedToReality
      neverEscaped <- hasRecordIn TheWebOfDreams TheInvestigatorsNeverEscaped
      stillInDreamlands <- hasRecordIn TheWebOfDreams TheInvestigatorsAreStillInTheDreamlands
      let reunited =
            (awoke && returned)
              || (stayed && stillInDreamlands)
              || (traveled && neverEscaped)
      when reunited $ earnDreamQuest ReunitedAndItFeelsSoGood

  -- "Only Way To Be Sure": The Thing in the Robes' ability 1 is the "shove the
  -- High Priest down the well" option. The campaign dispatches before the act, so
  -- the priest's damage is still on it.
  UseThisAbility _ source 1 | sourceIsAct Acts.theThingInTheRobes source -> do
    selectOne (enemyIs Enemies.highPriestNotToBeDescribed) >>= traverse_ \priest -> do
      remaining <- field EnemyRemainingHealth priest
      when (remaining == Just 1) $ earnDreamQuest OnlyWayToBeSure

  -- "Master of Unlocking": The Silver Key cancels exactly 1 horror per use, so
  -- ten uses in one scenario is ten horror.
  UseThisAbility _ source 1 | isJust source.asset -> do
    for_ source.asset \aid -> do
      assetDef <- fieldMap Asset.AssetCard toCardDef aid
      when (assetDef == Assets.theSilverKey) do
        n <- storedInt silverKeyHorrorKey
        setStore silverKeyHorrorKey (n + 1)
        when (n + 1 >= 10) $ earnWeb MasterOfUnlocking

  -- "Déjà Vu": every free triggered ability on every A Thousand Shapes of Horror
  -- location. Two pairs of those abilities normally lock each other out; with
  -- achievements on both stay offerable once each (see the location modules).
  UseThisAbility _ (sourceLocation -> Just lid) n -> whenScenarioIs aThousandShapesOfHorrorId do
    code <- fieldMap Location.LocationCard toCardCode lid
    when ((code, n) `elem` dejaVuAbilities) do
      used <- storedTexts dejaVuAbilitiesKey
      let used' = nub (dejaVuKey code n : used)
      setStore dejaVuAbilitiesKey used'
      when (length used' >= length dejaVuAbilities) $ earnWeb DejaVu

  -- "Don't Tell Anyone, But...": The Great Hall (the revealed Onyx Castle) is the
  -- only way a hidden card moves from one investigator's hand to another's, so a
  -- HiddenInHand placement whose card is ALREADY hidden in a different hand is
  -- that ability resolving. Drawing a hidden card is not a transfer: it has no
  -- prior HiddenInHand placement.
  PlaceTreachery tid (HiddenInHand target) -> whenScenarioIs whereTheGodsDwellId do
    placement <- field Treachery.TreacheryPlacement tid
    recordHiddenTransfer placement target (tshow tid)
  PlaceEnemy eid (HiddenInHand target) -> whenScenarioIs whereTheGodsDwellId do
    placement <- field EnemyPlacement eid
    recordHiddenTransfer placement target (tshow eid)

  -- "Bad Advice": flip over every Point of No Return location at least once.
  Flip _ _ (LocationTarget lid) -> whenScenarioIs pointOfNoReturnId do
    code <- fieldMap Location.LocationCard toCardCode lid
    when (code `elem` map toCardCode pointOfNoReturnLocations) do
      flipped <- storedList flippedLocationsKey
      let flipped' = nub (code : flipped)
      setStore flippedLocationsKey flipped'
      when (length flipped' >= length pointOfNoReturnLocations) $ earnWeb BadAdvice

  -- "March of the Ghouls": Richard Upton Pickman's ability 1 attaches a Ghoul by
  -- placing its card underneath him. The asset has not processed this placement
  -- yet, so the new cards are added to what is already there.
  PlaceUnderneath (AssetTarget aid) cards -> whenScenarioIs pointOfNoReturnId do
    assetDef <- fieldMap Asset.AssetCard toCardDef aid
    when (assetDef == Assets.richardUptonPickman) do
      existing <- fieldMap Asset.AssetCardsUnderneath (count isGhoul) aid
      let attached = existing + count isGhoul cards
      when (attached >= 4) $ earnWeb MarchOfTheGhouls

  -- "The Doctor is In" bookkeeping: any damage or horror token landing on Dr.
  -- Maheswaran disqualifies the earn.
  PlaceTokens _ (AssetTarget aid) tok n
    | n > 0 && tok `elem` [Token.Damage, Token.Horror] -> whenScenarioIs wakingNightmareId do
        assetDef <- fieldMap Asset.AssetCard toCardDef aid
        when (assetDef == Assets.drShivaniMaheswaran) $ setStore doctorHurtKey True

  -- "You Spin Me Right 'Round": both acts spin Atlach-Nacha by 45 degrees per
  -- point of a revealed token's negative modifier. A full turn is 8 of those.
  HandleAbilityOption _ (EnemySource eid) n | n > 0 -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (cardDef == Enemies.atlachNacha) do
      degrees <- storedInt atlachNachaSpinKey
      let degrees' = degrees + 45 * n
      setStore atlachNachaSpinKey degrees'
      when (degrees' >= 360) $ earnWeb YouSpinMeRightRound

  {- Act-advance detections. These want the scenario BEATEN, which cannot be read
  off surviving investigators: The Endless Stairs is beaten by every investigator
  resigning, so at the winning moment they are all eliminated. The campaign sees
  AdvanceAct before the act does and the board is still fully intact; the twin
  side-A/side-B dispatches only mean a harmless duplicate earn. -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.containingTheOutbreak -> do
    whenScenarioIs wakingNightmareId do
      -- "The Carter Method": a location only gains a horror token by being sealed,
      -- and it can only be sealed after becoming infested, so "every location
      -- sealed" is exactly "every location was infested, then sealed".
      locations <- select Anywhere
      unsealed <- select $ Anywhere <> not_ (LocationWithHorror $ atLeast 1)
      when (notNull locations && null unsealed) $ earnWeb TheCarterMethod
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.theEndlessStairs -> do
    whenScenarioIs aThousandShapesOfHorrorId do
      -- "The Casa Loma Maneuver": Endless Descent relabels the stairs as it
      -- rotates them, so the topmost one is always "mysteriousStairs1".
      atTop <-
        selectAny
          $ enemyIs Enemies.theUnnamable
          <> EnemyAt (LocationWithLabel $ mkLabel "mysteriousStairs1")
      when atTop $ earnWeb TheCasaLomaManeuver

  -- Scenario-end detections. EndOfGame is pushed by the resolution body after the
  -- Scenario wrapper's clearQueues, and the board is still fully intact when it
  -- dispatches (teardown happens at the EndOfScenario it queues).
  EndOfGame _ ->
    selectOne TheScenario >>= traverse_ \sid ->
      if
        | sid == beyondTheGatesOfSleepId -> do
            -- "Do You Always Follow Orders?": both The Path's forced ability and
            -- Journey Through the Gates' final advance record straying.
            unlessM (getHasRecord TheDreamersStrayedFromThePath)
              $ earnDreamQuest DoYouAlwaysFollowOrders
            -- "Aww, But They're So Cute".
            unlessM (storedFlag zoogDefeatedKey) $ earnDreamQuest AwwButTheyreSoCute
        | sid == wakingNightmareId -> do
            -- "The Doctor is In": she has to have come along in the first place.
            joined <- getHasRecord DrMaheswaranJoinedTheInvestigation
            hurt <- storedFlag doctorHurtKey
            when (joined && not hurt) $ earnWeb TheDoctorIsIn
        | sid == darkSideOfTheMoonId -> do
            -- "Tactical Espionage Action". Eliminated investigators are included:
            -- resigning does not clear an alarm level, and a winning Dark Side of
            -- the Moon regularly ends with investigators eliminated.
            iids <- select $ IncludeEliminated Anyone
            alarmLevels <- traverse getAlarmLevel iids
            when (notNull iids && all (== 0) alarmLevels)
              $ earnDreamQuest TacticalEspionageAction
        | otherwise -> pure ()
  _ -> pure ()
 where
  resetRoundTallies = do
    setStore legsDefeatedThisRoundKey ([] :: [CardCode])
    setStore nyarlathotepsThisRoundKey ([] :: [CardCode])

  recordHiddenTransfer placement target key = case placement of
    HiddenInHand holder | holder /= target -> do
      handedOver <- storedTexts hiddenCardsGivenKey
      let handedOver' = nub (key : handedOver)
      setStore hiddenCardsGivenKey handedOver'
      when (length handedOver' >= 6) $ earnDreamQuest DontTellAnyoneBut
    _ -> pure ()

earnDreamQuest :: (HasGame m, HasQueue Message m) => TheDreamQuestAchievement -> m ()
earnDreamQuest = earnAchievement . TheDreamQuestAchievement

earnWeb :: (HasGame m, HasQueue Message m) => TheWebOfDreamsAchievement -> m ()
earnWeb = earnAchievement . TheWebOfDreamsAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheDreamQuestAchievement DoYouAlwaysFollowOrders
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: (HasGame m, Tracing m) => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

getAlarmLevel :: (HasCallStack, HasGame m, Tracing m) => InvestigatorId -> m Int
getAlarmLevel = fieldMap InvestigatorTokens (Token.countTokens Token.AlarmLevel)

currentDifficulty :: HasGame m => m (Maybe Difficulty)
currentDifficulty = do
  g <- getGame
  pure $ campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

activeUltimatumCount :: HasGame m => m Int
activeUltimatumCount = do
  g <- getGame
  pure $ length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]

{- | Whether a record exists in one specific half's campaign log. In the full
campaign only one half's 'CampaignAttrs' is live at a time; the other rides in
the metadata. Mirrors the campaign runner's own helper of the same shape.
-}
hasRecordIn
  :: (IsCampaignLogKey k, HasGame m, Tracing m, HasCallStack) => CampaignPart -> k -> m Bool
hasRecordIn part key = do
  meta <- getCampaignMeta @Metadata
  if meta.currentCampaignMode == Just part
    then getHasRecord key
    else pure $ maybe False (hasRecord key . (.log)) meta.otherCampaignAttrs

sourceIsAct :: CardDef -> Source -> Bool
sourceIsAct def source = maybe False ((== toCardCode def) . unActId) (sourceAct source)

sourceAct :: Source -> Maybe ActId
sourceAct = \case
  ActSource aid -> Just aid
  AbilitySource s _ -> sourceAct s
  UseAbilitySource _ s _ -> sourceAct s
  IndexedSource _ s -> sourceAct s
  ProxySource s _ -> sourceAct s
  PaymentSource s -> sourceAct s
  _ -> Nothing

sourceLocation :: Source -> Maybe LocationId
sourceLocation = \case
  LocationSource lid -> Just lid
  AbilitySource s _ -> sourceLocation s
  UseAbilitySource _ s _ -> sourceLocation s
  IndexedSource _ s -> sourceLocation s
  ProxySource s _ -> sourceLocation s
  PaymentSource s -> sourceLocation s
  _ -> Nothing

isGhoul :: Card -> Bool
isGhoul = (`cardMatch` CardWithTrait Ghoul)

beyondTheGatesOfSleepId, wakingNightmareId, theSearchForKadathId :: ScenarioId
beyondTheGatesOfSleepId = "06039"
wakingNightmareId = "06063"
theSearchForKadathId = "06119"

aThousandShapesOfHorrorId, darkSideOfTheMoonId :: ScenarioId
aThousandShapesOfHorrorId = "06168"
darkSideOfTheMoonId = "06206"

pointOfNoReturnId, whereTheGodsDwellId :: ScenarioId
pointOfNoReturnId = "06247"
whereTheGodsDwellId = "06286"

{- | Where the Gods Dwell's five hidden forms. The True Shape shares their title
but is not one of them (it is what they become), so it is matched by card def
rather than by title.
-}
nyarlathotepForms :: [CardDef]
nyarlathotepForms =
  [ Enemies.nyarlathotepTheCrawlingChaos
  , Enemies.nyarlathotepTheFacelessWhisperer
  , Enemies.nyarlathotepMessengerOfTheOuterGods
  , Enemies.nyarlathotepGodOfAThousandForms
  , Enemies.nyarlathotepStalkerAmongTheStars
  ]

legsOfAtlachNacha :: [CardDef]
legsOfAtlachNacha =
  [ Enemies.legsOfAtlachNacha_347
  , Enemies.legsOfAtlachNacha_348
  , Enemies.legsOfAtlachNacha_349
  , Enemies.legsOfAtlachNacha_350
  ]

{- | Every flippable Point of No Return location; the Enchanted Woods (Stone
Trapdoor) that Another Path puts into play has no other side.
-}
pointOfNoReturnLocations :: [CardDef]
pointOfNoReturnLocations =
  [ Locations.vaultsOfZin
  , Locations.cityOfGugs
  , Locations.towerOfKoth
  , Locations.plainOfTheGhouls
  , Locations.cragOfTheGhouls
  , Locations.seaOfBones
  , Locations.peaksOfThok
  , Locations.valeOfPnath
  , Locations.seaOfPitch_262
  , Locations.seaOfPitch_263
  , Locations.seaOfPitch_264
  , Locations.seaOfPitch_265
  ]

{- | Every FREE triggered ability printed on an A Thousand Shapes of Horror
location, as (location card code, ability index). "Free triggered ability" is
the [fast] symbol specifically — a [reaction] is a triggered ability but not a
free one, so the Den's "after you successfully investigate, discard 1 card:
discover an additional clue" (ability 1) is deliberately NOT here. Neither is
the Bedroom's two-action ability (1), nor the Unmarked Tomb's forced objective.
The Mysterious Stairs are excluded by the achievement itself.
-}
dejaVuAbilities :: [(CardCode, Int)]
dejaVuAbilities =
  [ (toCardCode Locations.burialGround, 1)
  , (toCardCode Locations.frontPorchEntryway, 1)
  , (toCardCode Locations.frontPorchEntryway, 2)
  , (toCardCode Locations.downstairsDoorwayDen, 2)
  , (toCardCode Locations.downstairsDoorwayParlor, 1)
  , (toCardCode Locations.upstairsHallway, 1)
  , (toCardCode Locations.upstairsDoorwayLibrary, 1)
  , (toCardCode Locations.upstairsDoorwayBedroom, 2)
  , (toCardCode Locations.attic_AThousandShapesOfHorror, 1)
  ]

dejaVuKey :: CardCode -> Int -> Text
dejaVuKey code n = tshow code <> ":" <> tshow n

dreamQuestWins :: [TheDreamEatersKey]
dreamQuestWins =
  [ TheDreamersAwoke
  , TheDreamersStayedInTheDreamlandsForever
  , TheDreamersTraveledBeneathTheMonastery
  ]

-- | Every way The Dream-Quest can end, win or lose.
dreamQuestEndings :: [TheDreamEatersKey]
dreamQuestEndings = Nyarlathotep'sInvasionHasBegun : dreamQuestWins

webOfDreamsWins :: [TheDreamEatersKey]
webOfDreamsWins =
  [ TheInvestigatorsReturnedToReality
  , TheInvestigatorsNeverEscaped
  , TheInvestigatorsAreStillInTheDreamlands
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

zoogDefeatedKey
  , spidersDefeatedKey
  , doctorHurtKey
  , dejaVuAbilitiesKey
  , flippedLocationsKey
  , hiddenCardsGivenKey
  , legsDefeatedThisRoundKey
  , nyarlathotepsThisRoundKey
  , atlachNachaSpinKey
  , silverKeyHorrorKey
    :: Text
zoogDefeatedKey = "tdeAchZoogDefeated"
spidersDefeatedKey = "tdeAchSpidersDefeated"
doctorHurtKey = "tdeAchDoctorHurt"
dejaVuAbilitiesKey = "tdeAchDejaVuAbilities"
flippedLocationsKey = "tdeAchFlippedLocations"
hiddenCardsGivenKey = "tdeAchHiddenCardsGiven"
legsDefeatedThisRoundKey = "tdeAchLegsThisRound"
nyarlathotepsThisRoundKey = "tdeAchNyarlathotepsThisRound"
atlachNachaSpinKey = "tdeAchAtlachNachaSpin"
silverKeyHorrorKey = "tdeAchSilverKeyHorror"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. defeating a victory enemy) clearQueue, which
-- would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedList :: (HasCallStack, HasGame m, Tracing m) => Text -> m [CardCode]
storedList k = fromMaybe [] <$> stored k

storedTexts :: (HasCallStack, HasGame m, Tracing m) => Text -> m [Text]
storedTexts k = fromMaybe [] <$> stored k
