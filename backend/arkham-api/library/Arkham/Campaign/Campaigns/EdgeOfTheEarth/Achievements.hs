{- | Edge of the Earth achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign id
("08"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns, matching every other campaign's detection
module. Like The Drowned City and The Innsmouth Conspiracy, this list is printed
for the campaign itself, so there is no Return-to variant to inherit the hook.

Scenario-end detections key on 'EndOfGame', NOT on 'ScenarioResolution': the
Scenario wrapper clearQueues twice while processing a resolution, wiping even
Priority pushes made during that dispatch. 'EndOfGame' is what 'endOfScenario'
pushes afterwards, and the board is still intact when it dispatches. It doubles
as the "survived it" signal — The Heart of Madness, Part II's NoResolution
drives everyone insane and calls 'gameOver' instead, so it never gets here.
-}
module Arkham.Campaign.Campaigns.EdgeOfTheEarth.Achievements (
  runEdgeOfTheEarthAchievements,
) where

import Arkham.Achievement
import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignChaosBag, campaignDifficulty)
import Arkham.CampaignLog (PartnerStatus (Resolute))
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Seal (SealKind (..), sealKind)
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (FrostToken))
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.EncounterSet (EncounterSet (MemorialsOfTheLost, Tekelili))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Id
import Arkham.Key (ArkhamKey)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Location
import Arkham.Matcher hiding (PerformAction, PlaceUnderneath)
import Arkham.Message
import Arkham.Modifier (ModifierType (ScenarioModifier))
import Arkham.Movement (moveTarget)
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.IceAndDeath.Helpers (camps)
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

runEdgeOfTheEarthAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runEdgeOfTheEarthAchievements msg = whenEligibleCampaign $ case msg of
  {- "Safe Bet": camping is recording one of the Camp_* keys, which Ice and Death,
  Part I does at its resolution (or when the last investigator resigns). Each key
  names a location whose shelter value lives in its card def meta.
  -}
  Record key -> whenScenarioIs iceAndDeathPart1Id do
    for_ (fromCampaignLogKey key) \eoteKey ->
      for_ (campLocation eoteKey) \cardCode ->
        when (getShelterValue cardCode == Just 8) $ earn SafeBet

  {- "Look at All This Stuff!": every supply recovered in Ice and Death, carried all
  the way up. This fires the moment someone reaches The Summit with the full set,
  rather than waiting for the scenario to end. 'After' the move, not the move
  itself, so the investigator is already there and anything they are carrying has
  come with them.
  -}
  After (MoveTo movement) | InvestigatorTarget iid <- moveTarget movement ->
    whenScenarioIs toTheForbiddenPeaksId do
      whenM (iid <=~> InvestigatorAt atTheSummit) do
        whenM (allM supplyAtSummit supplyAssets) $ earn LookAtAllThisStuff

  {- "In Your Head": FINISH Fatal Mirage with nine story cards in the victory
  display - so this waits for the scenario to be over rather than firing the
  moment a ninth memory is banished.

  Shadow of the Past advancing to side B is that moment: it pushes the resolution.
  The campaign sees the advance first, which matters because the resolution then
  empties the victory display (it obtains every banished memory), so by 'EndOfGame'
  there is nothing left to count.
  -}
  AdvanceAct aid _ _ | unActId aid `elem` map toCardCode fatalMirageActs ->
    whenScenarioIs fatalMirageId do
      n <- selectCount $ VictoryDisplayCardMatch $ basic #story
      when (n >= 9) $ earn InYourHead

  {- "Chaos Chaos": ten keys collected AND ten keys spent - both halves, not ten
  events between them.

  Which keys are in hand is tracked here rather than read off the investigators:
  by the time the campaign sees a key placed on a location the investigator has
  ALREADY dropped it (the investigator runner removes the key on any PlaceKey not
  targeting them), so asking "does anyone hold this?" always answered no and
  spending never counted. Keys compare by chaos-token id, so keeping them in the
  store is stable.

  A spend is a held key going ANYWHERE that is not an investigator. Paying a
  SpendTokenKeyCost (the two-at-once costs most of the scenario's abilities use)
  pushes @PlaceKey ScenarioTarget@, not a location, so matching only locations
  missed every real spend.
  -}
  PlaceKey target k -> whenScenarioIs cityOfTheElderThingsId do
    collected <- storedInt keysCollectedKey
    spent <- storedInt keysSpentKey
    held <- storedKeys keysHeldKey
    -- The bumped values are used directly: setStore only queues the write, so
    -- reading the store back here would still see the pre-bump number.
    (collected', spent') <- case target of
      InvestigatorTarget _ -> do
        setStore keysCollectedKey (collected + 1)
        setStore keysHeldKey (nub (k : held))
        pure (collected + 1, spent)
      _ | k `elem` held -> do
        setStore keysSpentKey (spent + 1)
        setStore keysHeldKey (filter (/= k) held)
        pure (collected, spent + 1)
      _ -> pure (collected, spent)
    when (collected' >= 10 && spent' >= 10) $ earn ChaosChaos

  {- "Knock, Knock": all five seals collected, activated and placed. This fires as
  the last one goes down rather than at the end of the scenario. The campaign sees
  the placement before it lands, so the arriving seal is counted alongside the ones
  already on the board. Only seals placed on LOCATIONS count - a seal an
  investigator is still carrying has not been placed.
  -}
  PlaceSeal (LocationTarget _) seal | seal.active ->
    whenScenarioIs theHeartOfMadnessPart1Id do
      placed <- selectField Location.LocationSeals Anywhere
      let kinds = nub $ sealKind seal : [sealKind s | s <- concatMap toList placed, s.active]
      when (length kinds >= length [minBound @SealKind ..]) $ earn KnockKnock

  {- "Mad With Power": fifteen copies of The Nameless Madness exhausted at once.
  Fifteen are set aside in Part II, so the whole set being exhausted is the
  achievement; the campaign sees the exhaust before it lands, hence the +1.
  -}
  Exhaust _ -> whenScenarioIs theHeartOfMadnessPart2Id do
    n <- selectCount $ ExhaustedEnemy <> enemyIs Enemies.theNamelessMadness
    when (n + 1 >= 15) $ earn MadWithPower

  {- "Construct Additional Pylons" bookkeeping: a Mist-Pylon counts as collapsed
  while it carries enough damage (a derived ScenarioModifier), and advancing
  Collapse the Pylons is what needs all five. The flag is latched because the
  advance removes the locations, so the count cannot be taken again later.
  -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.collapseThePylons ->
    whenScenarioIs theHeartOfMadnessPart2Id do
      collapsed <- selectCount $ LocationWithModifier $ ScenarioModifier "collapsed"
      when (collapsed >= 5) $ setStore pylonsCollapsedKey True

  {- "The Sound of Madness": ten Tekeli-li! drawn in a single game.

  Tekeli-li are treacheries shuffled INTO a player deck, so they arrive through
  the ordinary treachery-draw path. Neither the Tekeli-li scenario deck nor
  'DrewCards' sees them: 'DrewCards' is only emitted for targeted draws (every
  consumer filters on its target), while a plain draw pushes 'DrewTreachery'.
  -}
  DrewTreachery _ _ card | isTekelili card -> do
    drawn <- storedInt tekeliliDrawnKey
    setStore tekeliliDrawnKey (drawn + 1)
    when (drawn + 1 >= 10) $ earn TheSoundOfMadness

  {- "This Was Your Idea": four horror healed off Danforth by Dyer's ability in one
  scenario. Dyer heals two at a time, so this is two uses on Danforth.
  -}
  HealHorror (AssetTarget aid) source n | n > 0 -> do
    whenM (sourceIsDyersAbility source) do
      cardCode <- field Asset.AssetCardCode aid
      when (cardCode `elem` danforthCodes) do
        healed <- storedInt dyerHealedDanforthKey
        setStore dyerHealedDanforthKey (healed + n)
        when (healed + n >= 4) $ earn ThisWasYourIdea

  {- "Wuk Wuk Boom": one Dynamite blast defeating two Giant Albino Penguins. The
  counter is reset as the ability is used, so only penguins killed by the same
  blast are counted together.
  -}
  UseThisAbility _ source 1 ->
    whenM (isDynamiteSource source) $ setStore dynamitePenguinsKey (0 :: Int)
  Defeated (EnemyTarget eid) _ source _ -> whenM (isDynamiteSource source) do
    cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
    when (cardDef == Enemies.giantAlbinoPenguin) do
      n <- storedInt dynamitePenguinsKey
      setStore dynamitePenguinsKey (n + 1)
      when (n + 1 >= 2) $ earn WukWukBoom

  {- "Kind of a Hat on a Hat". The printed wording is strict: play a Wooden Sledge
  out of a Backpack, and then the very NEXT action you take must be that Sledge's
  ability, attaching another Backpack to it. Free triggers and reactions in
  between are fine - only taking a different ACTION breaks the chain.

  Three steps, in order:
    1. the Sledge is played while sitting underneath a Backpack (the campaign sees
       the play before the Backpack drops the card, so it is still underneath here)
    2. it reaches play, which arms the chain
    3. the next ACTION must be the Sledge's own; anything else disarms

  Actions are counted off 'TakenActions', which ActiveCost pushes as every action
  completes. 'PerformAction' looked like the natural hook but is only emitted for
  a few built-in action types, so ordinary actions slipped past it and the chain
  never broke. The first TakenActions after arming is the Sledge's own play; the
  payoff lands during the Sledge's ability, before that ability's own
  TakenActions, so a second completed action means something else intervened.
  -}
  InitiatePlayCard _ card _ _ _ _ | toCardDef card == Assets.woodenSledge -> do
    underBackpack <- cardIsUnderABackpack card
    when underBackpack $ setStore sledgePlayedFromBackpackKey (toJSON $ toCardId card)
  CardEnteredPlay _ card | toCardDef card == Assets.woodenSledge -> do
    pending <- stored sledgePlayedFromBackpackKey
    when (pending == Just (toCardId card)) do
      setStore sledgeChainArmedKey True
      setStore sledgeActionsKey (0 :: Int)
  -- Any completed action beyond the Sledge's own play breaks the chain.
  TakenActions _ _ -> whenM (storedFlag sledgeChainArmedKey) do
    n <- storedInt sledgeActionsKey
    setStore sledgeActionsKey (n + 1)
    when (n + 1 > 1) $ setStore sledgeChainArmedKey False
  -- The payoff: that Sledge attaching another Backpack to itself, still on chain.
  PlaceUnderneath (AssetTarget aid) cards -> do
    -- fieldMay, not field: PlaceUnderneath is also used on assets that are not in
    -- play (or are mid-removal), and a hard projection there throws.
    mCardDef <- fmap toCardDef <$> fieldMay Asset.AssetCard aid
    when (mCardDef == Just Assets.woodenSledge && any ((`elem` backpacks) . toCardDef) cards) do
      whenM (storedFlag sledgeChainArmedKey) $ earn KindOfAHatOnAHat

  {- Board-state achievements, re-checked whenever an asset arrives. Both want a
  simultaneous state rather than a cumulative one, so they are recomputed rather
  than tallied.
  -}
  CardEnteredPlay iid _ -> checkAssetBoard iid
  TakeControlOfAsset iid _ -> checkAssetBoard iid
  {- "Abandoned and Alone" / "Friends Forever" bookkeeping. Taking a partner into a
  scenario is the scenario handing the investigator that partner's card code; every
  Edge of the Earth scenario offers the choice at PreScenarioSetup.
  -}
  HandleTargetChoice _ ScenarioSource (CardCodeTarget cardCode) ->
    for_ (toPartnerCodeMay cardCode) \partnerCode -> do
      setStore broughtAPartnerKey True
      brought <- storedCodes partnersBroughtKey
      setStore partnersBroughtKey (nub (partnerCode : brought))
      selectOne TheScenario >>= traverse_ \sid -> do
        withPartner <- storedScenarios scenariosWithPartnerKey
        setStore scenariosWithPartnerKey (nub (sid : withPartner))

  -- Per-scenario counters reset as their scenario is set up; the scenario is also
  -- recorded so "brought a partner into every scenario" can be checked later.
  Setup -> do
    setStore tekeliliDrawnKey (0 :: Int)
    setStore dyerHealedDanforthKey (0 :: Int)
    setStore sledgeChainArmedKey False
    setStore sledgeActionsKey (0 :: Int)
    setStore keysCollectedKey (0 :: Int)
    setStore keysSpentKey (0 :: Int)
    setStore keysHeldKey ([] :: [ArkhamKey])
    setStore pylonsCollapsedKey False
    selectOne TheScenario >>= traverse_ \sid -> do
      played <- storedScenarios scenariosPlayedKey
      setStore scenariosPlayedKey (nub (sid : played))

  -- Scenario-end detections. See the module header for why these hang off
  -- EndOfGame rather than the resolution.
  -- "Construct Additional Pylons": all five down, and you got out alive. This is
  -- the one detection still keyed on the scenario ending, because escaping alive
  -- is exactly what EndOfGame means here.
  EndOfGame _ -> whenScenarioIs theHeartOfMadnessPart2Id do
    whenM (storedFlag pylonsCollapsedKey) $ earn ConstructAdditionalPylons
  {- End of campaign. The epilogue pushes GameOver itself, and the losing endings
  never reach it, so arriving here is both "completed" and "won".
  -}
  CampaignStep EpilogueStep -> do
    g <- getGame
    let campaign' = currentCampaign (gameMode g)
    let difficulty = campaignDifficulty . toAttrs <$> campaign'

    -- "The Cold Never Bothered Me Anyway" / "Hell Froze Over": the chaos bag at the
    -- end of the campaign lives on the campaign, the scenario having been torn down.
    let frost = count (== FrostToken) $ maybe [] (campaignChaosBag . toAttrs) campaign'
    when (frost >= 8) $ earn TheColdNeverBotheredMeAnyway
    when (frost == 0) $ earn HellFrozeOver

    -- "Abandoned and Alone": never took a partner into a scenario.
    unlessM (storedFlag broughtAPartnerKey) $ earn AbandonedAndAlone

    {- "Friends Forever": the same partner every scenario, resolute (they confronted
    their demons), and still alive at the end. Resolute IS alive, so no separate
    liveness check is needed.
    -}
    brought <- storedCodes partnersBroughtKey
    played <- storedScenarios scenariosPlayedKey
    withPartner <- storedScenarios scenariosWithPartnerKey
    case brought of
      [only'] | all (`elem` withPartner) played -> do
        status <- getPartnerStatus only'
        when (status == Resolute) $ earn FriendsForever
      _ -> pure ()

    -- "Line in the…Snow": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn SnowLineInTheSand

    -- "Antarctic Expertise": win on Expert.
    when (difficulty == Just Expert) $ earn AntarcticExpertise

    {- "There and Back Again": the checklist of expedition members who came home,
    accumulated across playthroughs by the API layer. The final scenario records
    every surviving partner (and investigator) in this set.
    -}
    survivors <- getRecordSet TheSurvivorsOfTheExpeditionWere
    let came (def, _) = recorded (toCardCode def) `elem` survivors
    achievementProgress (EdgeOfTheEarthAchievement ThereAndBackAgain)
      $ map snd
      $ filter came survivorItems
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => EdgeOfTheEarthAchievement -> m ()
earn = earnAchievement . EdgeOfTheEarthAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ EdgeOfTheEarthAchievement SafeBet
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: (HasGame m, Tracing m) => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

iceAndDeathPart1Id, fatalMirageId, toTheForbiddenPeaksId :: ScenarioId
iceAndDeathPart1Id = "08501a"
fatalMirageId = "08549"
toTheForbiddenPeaksId = "08596"

cityOfTheElderThingsId, theHeartOfMadnessPart1Id, theHeartOfMadnessPart2Id :: ScenarioId
cityOfTheElderThingsId = "08621"
theHeartOfMadnessPart1Id = "08648a"
theHeartOfMadnessPart2Id = "08648b"

{- | The location a camp record names. 'camps' maps the other way (location card
code to key), so it is inverted here.
-}
campLocation :: EdgeOfTheEarthKey -> Maybe CardCode
campLocation k = listToMaybe [cc | (cc, k') <- Map.toList camps, k' == k]

-- | The seven Ice and Death supplies, as the asset each becomes on the mountain.
supplyAssets :: [CardDef]
supplyAssets =
  [ Assets.greenSoapstoneJinxedIdol
  , Assets.woodenSledge
  , Assets.dynamite
  , Assets.miasmicCrystalStrangeEvidence
  , Assets.mineralSpecimen
  , Assets.smallRadio
  , Assets.spareParts
  ]

{- | Fatal Mirage's act, whichever version is in play; advancing it ends the
scenario.
-}
fatalMirageActs :: [CardDef]
fatalMirageActs = [Acts.shadowOfThePastV1, Acts.shadowOfThePastV2, Acts.shadowOfThePastV3]

-- | Whether a drawn card is one of the Tekeli-li! cards.
isTekelili :: Card -> Bool
isTekelili = (== Just Tekelili) . cdEncounterSet . toCardDef

-- | The Summit, the top of the mountain in To the Forbidden Peaks.
atTheSummit :: LocationMatcher
atTheSummit = locationIs Locations.theSummit

{- | Whether a supply has made it to the top: either sitting on The Summit, or
still in the hands of somebody standing there.
-}
supplyAtSummit :: (HasGame m, Tracing m) => CardDef -> m Bool
supplyAtSummit def =
  selectAny
    $ assetIs def
    <> oneOf [AssetAt atTheSummit, AssetControlledBy (InvestigatorAt atTheSummit)]

-- | Both printings of Danforth, whose horror "This Was Your Idea" counts.
danforthCodes :: [CardCode]
danforthCodes =
  [ toCardCode Assets.danforthBrilliantStudent
  , toCardCode Assets.danforthBrilliantStudentResolute
  ]

-- | Backpack and its upgrade; either satisfies "a Backpack".
backpacks :: [CardDef]
backpacks = [Assets.backpack, Assets.backpack2]

{- | Whether a source is one of these assets' abilities.

An asset ability is sourced as 'AssetSource', not 'CardCodeSource', so the card
code has to be read off the asset. @source.asset@ already peels the
ability/payment wrappers. Getting this wrong silently disabled Wuk Wuk Boom,
This Was Your Idea and Kind of a Hat on a Hat, all three of which look for an
asset's own ability.
-}
sourceIsAsset :: (HasGame m, Tracing m) => [CardDef] -> Source -> m Bool
sourceIsAsset defs source = case source.asset of
  Nothing -> pure False
  Just aid -> do
    mCardCode <- fieldMay Asset.AssetCardCode aid
    pure $ maybe False (`elem` map toCardCode defs) mCardCode

-- | Professor William Dyer's ability; both printings heal the same way.
sourceIsDyersAbility :: (HasGame m, Tracing m) => Source -> m Bool
sourceIsDyersAbility =
  sourceIsAsset
    [ Assets.professorWilliamDyerProfessorOfGeology
    , Assets.professorWilliamDyerProfessorOfGeologyResolute
    ]

isDynamiteSource :: (HasGame m, Tracing m) => Source -> m Bool
isDynamiteSource = sourceIsAsset [Assets.dynamite]

-- | Whether a card is currently sitting underneath a Backpack in play.
cardIsUnderABackpack :: (HasGame m, Tracing m) => Card -> m Bool
cardIsUnderABackpack card = do
  packs <- select $ mapOneOf assetIs backpacks
  undernearth <- traverse (field Asset.AssetCardsUnderneath) packs
  pure $ any (any ((== toCardId card) . toCardId)) undernearth

{- | Recheck the two "have N of these in play at once" achievements for an
investigator who just gained an asset.
-}
checkAssetBoard :: (HasGame m, HasQueue Message m, Tracing m) => InvestigatorId -> m ()
checkAssetBoard iid = do
  controlled <- select $ assetControlledBy iid
  defs <- traverse (fieldMap Asset.AssetCard toCardDef) controlled
  names <- traverse (fieldMap Asset.AssetName toTitle) controlled

  -- "Sorry, I'm All Out of Dog Puns": Anyu plus four other Dog-titled assets.
  let hasAnyu = Assets.anyuFaithfulCompanion `elem` defs
  let dogs = length (filter ("Dog" `T.isInfixOf`) names)
  when (hasAnyu && dogs >= 4) $ earn SorryImAllOutOfDogPuns

  -- "No Respect For the Dead": five assets from the Memorials of the Lost set.
  let memorials = length (filter ((== Just MemorialsOfTheLost) . cdEncounterSet) defs)
  when (memorials >= 5) $ earn NoRespectForTheDead

{- | Each expedition member paired with their checklist item key, in
'achievementChecklist' order.
-}
survivorItems :: [(CardDef, Text)]
survivorItems =
  [ (Assets.drAmyKenslerProfessorOfBiology, "DrAmyKensler")
  , (Assets.professorWilliamDyerProfessorOfGeology, "ProfWilliamDyer")
  , (Assets.danforthBrilliantStudent, "Danforth")
  , (Assets.jamesCookieFredericksDubiousChoice, "JamesCookieFredericks")
  , (Assets.eliyahAshevakDogHandler, "EliyahAshevak")
  , (Assets.drMalaSinhaDaringPhysician, "DrMalaSinha")
  , (Assets.takadaHirokoAeroplaneMechanic, "TakadaHiroko")
  , (Assets.averyClaypoolAntarcticGuide, "AveryClaypool")
  , (Assets.roaldEllsworthIntrepidExplorer, "RoaldEllsworth")
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

tekeliliDrawnKey
  , dyerHealedDanforthKey
  , dynamitePenguinsKey
  , sledgePlayedFromBackpackKey
  , sledgeChainArmedKey
  , sledgeActionsKey
  , keysCollectedKey
  , keysSpentKey
  , keysHeldKey
  , pylonsCollapsedKey
  , broughtAPartnerKey
  , partnersBroughtKey
  , scenariosWithPartnerKey
  , scenariosPlayedKey
    :: Text
tekeliliDrawnKey = "eoteAchTekeliliDrawn"
dyerHealedDanforthKey = "eoteAchDyerHealedDanforth"
dynamitePenguinsKey = "eoteAchDynamitePenguins"
sledgePlayedFromBackpackKey = "eoteAchSledgePlayedFromBackpack"
sledgeChainArmedKey = "eoteAchSledgeChainArmed"
sledgeActionsKey = "eoteAchSledgeActions"
keysCollectedKey = "eoteAchKeysCollected"
keysSpentKey = "eoteAchKeysSpent"
keysHeldKey = "eoteAchKeysHeld"
pylonsCollapsedKey = "eoteAchPylonsCollapsed"
broughtAPartnerKey = "eoteAchBroughtAPartner"
partnersBroughtKey = "eoteAchPartnersBrought"
scenariosWithPartnerKey = "eoteAchScenariosWithPartner"
scenariosPlayedKey = "eoteAchScenariosPlayed"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. an act advancing) clearQueue, which would
-- otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedKeys :: (HasCallStack, HasGame m, Tracing m) => Text -> m [ArkhamKey]
storedKeys k = fromMaybe [] <$> stored k

storedCodes :: (HasCallStack, HasGame m, Tracing m) => Text -> m [CardCode]
storedCodes k = fromMaybe [] <$> stored k

storedScenarios :: (HasCallStack, HasGame m, Tracing m) => Text -> m [ScenarioId]
storedScenarios k = fromMaybe [] <$> stored k
