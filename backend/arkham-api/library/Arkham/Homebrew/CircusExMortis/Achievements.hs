{- | Circus Ex Mortis achievement detection (campaign guide p38).

Hooked from the campaign's runMessage, so it sees every message BEFORE the
scenario and the entities: a defeated enemy is still in play, a released chaos
token is still on the card it was sealed on, and an investigator's resources
have not yet been bumped.

'earnAchievement' self-gates on the achievements setting and on the campaign id
(":circus-ex-mortis"), so earns here stay unconditional; the module as a whole
is gated by 'whenEligibleCampaign' so its store writes stay out of other
campaigns.

Scenario-end detections key on what a resolution WRITES, or on the final act's
'AdvanceAct', never on 'ScenarioResolution' itself — the Scenario wrapper
clearQueues twice while processing one, wiping even Priority pushes.
-}
module Arkham.Homebrew.CircusExMortis.Achievements (
  runCircusExMortisAchievements,
) where

import Arkham.Achievement
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.EffectMetadata (EffectMetadata (EffectModifiers))
import Arkham.Enemy.CardDefs.Promo qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.GameEnv (getCard, getSkillTest)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, inSkillTest)
import Arkham.Helpers.Source (getSourceController)
import Arkham.Homebrew.CircusExMortis.AchievementDefs
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Skills qualified as HBSkills
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.CircusExMortis.Helpers (allVices, getVices)
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Creature, Performer))
import Arkham.Treachery.CardDefs.Standalone qualified as Treacheries
import Arkham.Treachery.CardDefs.TheDunwichLegacy qualified as Treacheries
import Arkham.Treachery.Types (Field (TreacheryCard))
import Data.Aeson.Key qualified as Key

runCircusExMortisAchievements :: (HasGame m, HasQueue Message m) => Message -> m ()
runCircusExMortisAchievements msg = whenEligibleCampaign $ case msg of
  Defeated (EnemyTarget eid) _ source _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    {- "Scapegoat": Sacrificial Beast is Jenny Barnes' signature weakness, so its
    presence already implies the investigator gate. Terrified Captives only ever
    exists attached to a location. -}
    when (isPrintingOfAny [Enemies.sacrificialBeast] cardDef) do
      whenM (eid <=~> EnemyAt (LocationWithAsset (assetIs HBAssets.terrifiedCaptives))) do
        earn Scapegoat

    {- "Destined Karma": the killing test must be the one Amalthea's ability gave
    +3 to. The test is still live here (the defeat cascade runs inside ST.7). -}
    when (isPrintingOfAny [HBEnemies.theBlackGoat] cardDef) do
      mSid <- fmap (.id) <$> getSkillTest
      marked <- stored karmaTestKey
      when (isJust mSid && marked == fmap tshow mSid) $ earn DestinedKarma

    -- "Natural Selection": the killing source must be an ability on a Creature
    -- asset the defeating investigator controls.
    when (isPrintingOfAny [HBEnemies.newMoonBeastTamer] cardDef) do
      for_ (abilityAsset source) \aid -> do
        getSourceController source >>= traverse_ \iid -> do
          whenM (aid <=~> (AssetWithTrait Creature <> assetControlledBy iid)) do
            earn NaturalSelection

  -- "Clown College": evaded with Disguise's own evade ability.
  Successful (Action.Evade, EnemyTarget eid) _ source _ _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    for_ source.asset \aid -> do
      assetDef <- fieldMap Asset.AssetCard toCardDef aid
      when
        (isPrintingOfAny [HBEnemies.newMoonClown] cardDef && isPrintingOfAny [Assets.disguise] assetDef)
        do
          earn ClownCollege

  {- "Moonlight Sonata": Final Rhapsody (Jim Culver's signature weakness, so the
  investigator gate is implied) draws its five tokens in one request rather than
  a skill test, and the whole set arrives on this one message. -}
  RequestedChaosTokens source _ tokens -> do
    for_ source.treachery \tid -> do
      cardDef <- fieldMap TreacheryCard toCardDef tid
      when (isPrintingOfAny finalRhapsodies cardDef && countMoons tokens >= 3) $ earn MoonlightSonata

  -- Asset board states. The campaign runs before the asset, so the arriving card
  -- is not in play yet: check it against the OTHER half of each pair.
  CardEnteredPlay iid card -> checkAssetPairs iid (toCardDef card)
  TakeControlOfAsset iid aid -> checkAssetPairs iid =<< fieldMap Asset.AssetCard toCardDef aid
  PlaceAsset aid _ -> do
    cardDef <- fieldMap Asset.AssetCard toCardDef aid
    field Asset.AssetController aid >>= \case
      Just iid -> checkAssetPairs iid cardDef
      Nothing -> checkGravesideChat cardDef

  {- "Wolf of Wall Street": the resources are only on the investigator after this
  message is processed, so the incoming amount is added by hand. -}
  PlaceResources _ (InvestigatorTarget iid) n -> do
    current <- field InvestigatorResources iid
    when (current + n >= 20) $ checkWolfOfWallStreet iid

  -- "Time Out!": De Cultus Bestiae's action ability (index 1) is shared by every
  -- version of the book.
  UseCardAbility iid source 1 _ _ -> do
    for_ source.asset \aid -> do
      cardDef <- fieldMap Asset.AssetCard toCardDef aid
      when (isPrintingOfAny deCultusBestiaeVersions cardDef) do
        engaged <- selectCount (enemyEngagedWith iid)
        when (engaged >= 3) $ earn TimeOut

  {- "Destined Karma" bookkeeping: Amalthea's ability grants
  @AnySkillValue ((sealed moons at your location + 1) `div` 2)@, so +3 means five
  or six moons. Mark the test it was granted to; the defeat branch compares it. -}
  CreateWindowModifierEffect _ (EffectModifiers mods) source _
    | any ((== AnySkillValue 3) . modifierType) mods -> do
        for_ source.asset \aid -> do
          cardDef <- fieldMap Asset.AssetCard toCardDef aid
          when (isPrintingOfAny amaltheaWeavers cardDef) do
            mSid <- fmap (.id) <$> getSkillTest
            for_ mSid $ setStore karmaTestKey . tshow

  {- Moon tokens revealed during a skill test. The skill test's revealed list is
  already complete when the per-token reveals dispatch, so "four or more" needs
  no counter of its own. -}
  RevealChaosToken _ _ token | token.face == MoonToken -> do
    revealed <- getSkillTestRevealedChaosTokens
    when (countMoons revealed >= 4) $ earn ShootTheMoon

  {- "Wax and Wane" counts Invocation of Diana's two halves within one test: its
  HasModifiersFor cancels each moon as it resolves, and its post-test option
  releases up to two. -}
  ResolveChaosToken _ MoonToken _ -> whenInvocationOfDiana $ bumpCounter waxCancelsKey 1
  UnsealChaosToken token | token.face == MoonToken -> do
    {- The release is tied to the test the skill cancelled moons in, not to the
    skill still being in play: its release option resolves off PassedSkillTest,
    behind a prompt, by which point the committed card can already be gone. -}
    whenM inSkillTest do
      whenM ((> 0) <$> storedInt waxCancelsKey) $ bumpCounter waxReleasesKey 1

    -- "Utter Lunatic": six released from one investigator card in one round. The
    -- token is still sealed on its card at this point.
    selectOne (InvestigatorWithSealedChaosToken (chaosTokenIs token)) >>= traverse_ \iid ->
      bumpCounter (lunaticKey iid) 1

  {- "Lactose Intolerant": cancelling a treachery's revelation always goes through
  CancelEachNext carrying the card id, whether by cancelRevelation or
  cancelCardEffects. -}
  CancelEachNext (Just cid) _ msgTypes | RevelationMessage `elem` msgTypes -> do
    card <- getCard cid
    when (isPrintingOfAny [HBTreacheries.milkOfShubNiggurath] card) $ bumpCounter milkCancelsKey 1

  -- "G.O.A.T.": AssignedDamage carries the post-modifier amount that is about to
  -- land; Shub-Niggurath's damage is wiped every round by The Prophecy.
  AssignedDamage (EnemyTarget eid) _ n _ | n > 0 -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (isPrintingOfAny [HBEnemies.shubNiggurath] cardDef) $ bumpCounter shubDamageKey n

  -- "Deep Sleepers" bookkeeping: any Towering Dark Young attack disqualifies it.
  EnemyAttack details -> do
    whenM (details.enemy <=~> EnemyWithTitle "Towering Dark Young") do
      setStore toweringAttackKey True

  {- "Deep Sleepers": Harm's Way R1 and R2 record the same count key and both
  chain to R3, so the only signal for "reached Resolution 2" is the act that
  pushes it. -}
  AdvanceAct aid _ _ | unActId aid == toCardCode HBActs.overdueDeparture -> do
    whenScenarioIs harmsWayId do
      unlessM (storedFlag toweringAttackKey) $ earn DeepSleepers

  -- "Vice Squad": Bacchanalia R2's record. Vices are scenario log keys, so they
  -- are still readable here; an investigator killed mid-scenario still chose.
  Record key | key == toCampaignLogKey TheInvestigatorsDiscoveredTheRitualsLocation -> do
    iids <- select $ IncludeEliminated Anyone
    vices <- traverse getVices iids
    when (notNull iids && all ((== length allVices) . length) vices) $ earn ViceSquad

  -- The two checklists. A playthrough grants exactly one final version of each
  -- book, so the items accumulate per user across campaigns.
  AddCampaignCardToDeck _ _ card -> do
    for_ (checklistItem amaltheaFinals card) \item ->
      achievementProgress (ach ManyFutures) [item]
    for_ (checklistItem deCultusFinals card) \item ->
      achievementProgress (ach ManyPasts) [item]

  -- Per-game and per-round counters reset at their boundaries.
  Setup -> do
    setStore milkCancelsKey (0 :: Int)
    whenScenarioIs harmsWayId $ setStore toweringAttackKey False
    whenScenarioIs allPointsWestId $ setStore traumaAtSetupKey =<< totalTrauma
  BeginRound -> do
    setStore shubDamageKey (0 :: Int)
    iids <- select Anyone
    for_ iids \iid -> setStore (lunaticKey iid) (0 :: Int)
  BeforeSkillTest _ -> do
    setStore waxCancelsKey (0 :: Int)
    setStore waxReleasesKey (0 :: Int)
  EndRound -> do
    iids <- select $ IncludeEliminated Anyone
    whenM (anyM (\iid -> (>= 6) <$> storedInt (lunaticKey iid)) iids) $ earn UtterLunatic

  -- "Pain Train": trauma earned over the course of All Points West. Defeat and
  -- being driven insane apply trauma without a message, so it is a difference of
  -- two snapshots rather than a tally.
  EndOfGame _ -> whenScenarioIs allPointsWestId do
    before <- storedInt traumaAtSetupKey
    now <- totalTrauma
    when (now - before >= 3) $ earn PainTrain

  {- End of campaign. Every surviving ending routes through the epilogue; both
  total losses call gameOver instead and never reach it. -}
  CampaignStep EpilogueStep -> do
    -- "Steal the Show": only Performer investigators were brought along.
    anyOther <- selectAny $ IncludeEliminated (not_ (InvestigatorWithTrait Performer))
    unless anyOther $ earn StealTheShow

    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    when (difficulty == Just Expert) $ earn GreatestShowOnEarth

  -- Deferred threshold checks: bumpCounter does its arithmetic when processed, so
  -- the stored value only reads back correctly on the trailing Do.
  CounterBumped k
    | k == milkCancelsKey -> whenM ((>= 2) <$> storedInt k) $ earn LactoseIntolerant
    | k == shubDamageKey -> do
        threshold <- perPlayer 8
        whenM ((>= threshold) <$> storedInt k) $ earn GOAT
    | k `elem` [waxCancelsKey, waxReleasesKey] -> do
        cancels <- storedInt waxCancelsKey
        releases <- storedInt waxReleasesKey
        when (cancels >= 1 && releases >= 2) $ earn WaxAndWane
  _ -> pure ()

ach :: CircusExMortisAchievement -> Achievement
ach = homebrewAchievement achievementCampaign . tshow

earn :: (HasGame m, HasQueue Message m) => CircusExMortisAchievement -> m ()
earn = earnAchievement . ach

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  when (maybe False (`elem` achievementCampaigns (ach Scapegoat)) mCampaignId) body

whenScenarioIs :: HasGame m => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

harmsWayId, allPointsWestId :: ScenarioId
harmsWayId = ":circus-ex-mortis:040"
allPointsWestId = ":circus-ex-mortis:074"

-- | Only count a moon cancel/release while Invocation of Diana is committed.
whenInvocationOfDiana :: HasGame m => m () -> m ()
whenInvocationOfDiana body =
  whenM (selectAny $ skillIs HBSkills.invocationOfDiana) body

{- | True when the card is any printing of one of these defs.

Not a structural 'CardDef' comparison: 'Arkham.Card.PlayerCard.toCardDef' runs
the card's taboo list over the def, so a tabooed copy of a player card is no
longer equal to the pristine def -- and alternate and replacement printings
(Baron Samedi's 99003, the campaign's own Lady Esprit) have codes of their own.
Taboo never touches the card code, which is what this compares.
-}
isPrintingOfAny :: (HasCardCode a, HasCardDef a) => [CardDef] -> a -> Bool
isPrintingOfAny defs x = any ((`isPrintingOf` x) . toCardCode) defs

-- | The checklist item a card stands for, by printing.
checklistItem :: (HasCardCode a, HasCardDef a) => [(CardDef, Text)] -> a -> Maybe Text
checklistItem items x = snd <$> find (\(def, _) -> isPrintingOf (toCardCode def) x) items

countMoons :: [ChaosToken] -> Int
countMoons = count ((== MoonToken) . (.face))

{- | Strictly "an ability on this asset", unlike @source.asset@, which also
unwraps a bare asset source (non-ability damage).
-}
abilityAsset :: Source -> Maybe AssetId
abilityAsset = \case
  AbilitySource s _ -> s.asset
  UseAbilitySource _ s _ -> s.asset
  PaymentSource s -> abilityAsset s
  IndexedSource _ s -> abilityAsset s
  ProxySource s _ -> abilityAsset s
  _ -> Nothing

totalTrauma :: HasGame m => m Int
totalTrauma = do
  iids <- select $ IncludeEliminated Anyone
  sum <$> for iids \iid -> do
    physical <- field InvestigatorPhysicalTrauma iid
    mental <- field InvestigatorMentalTrauma iid
    pure (physical + mental)

-- "Have X and Y in play at the same time" checked as the second one arrives.
checkAssetPairs :: (HasGame m, HasQueue Message m) => InvestigatorId -> CardDef -> m ()
checkAssetPairs iid cardDef = do
  checkGravesideChat cardDef
  when (isJust (otherHalf wolfOfWallStreetPair cardDef)) do
    whenM ((>= 20) <$> field InvestigatorResources iid) $ checkWolfOfWallStreet' iid cardDef

{- "Graveside Chat": Marie Lambeau's own weakness ally plus the Bokor. Lady
Esprit is an uncontrolled story asset, so neither half is required to be
controlled. 'InvestigatorWithTitle' covers every Marie printing. -}
checkGravesideChat :: (HasGame m, HasQueue Message m) => CardDef -> m ()
checkGravesideChat cardDef =
  for_ (otherHalf gravesideChatPair cardDef) \other -> do
    whenM (selectAny $ assetIs other) do
      whenM (selectAny $ IncludeEliminated (InvestigatorWithTitle "Marie Lambeau")) do
        earn GravesideChat

checkWolfOfWallStreet :: (HasGame m, HasQueue Message m) => InvestigatorId -> m ()
checkWolfOfWallStreet iid =
  whenM (allM (\def -> selectAny $ assetIs def <> assetControlledBy iid) wolfOfWallStreetPair) do
    earn WolfOfWallStreet

-- The arriving asset is not in play yet, so only the other half is queried.
checkWolfOfWallStreet' :: (HasGame m, HasQueue Message m) => InvestigatorId -> CardDef -> m ()
checkWolfOfWallStreet' iid cardDef =
  for_ (otherHalf wolfOfWallStreetPair cardDef) \other -> do
    whenM (selectAny $ assetIs other <> assetControlledBy iid) $ earn WolfOfWallStreet

gravesideChatPair, wolfOfWallStreetPair :: [CardDef]
gravesideChatPair = [Assets.baronSamedi, Assets.ladyEsprit]
wolfOfWallStreetPair = [Assets.monstrousTransformation, Assets.loneWolf]

{- | The other half of a pair, when the given def is one of them. Matched by
printing (Circus Ex Mortis reprints Lady Esprit under its own card code), which
is also what 'assetIs' does on the other side.
-}
otherHalf :: [CardDef] -> CardDef -> Maybe CardDef
otherHalf pair cardDef = case partition (\def -> isPrintingOf (toCardCode def) cardDef) pair of
  ([_], [other]) -> Just other
  _ -> Nothing

finalRhapsodies :: [CardDef]
finalRhapsodies = [Treacheries.finalRhapsody, Treacheries.finalRhapsodyAdvanced]

amaltheaWeavers :: [CardDef]
amaltheaWeavers =
  [ HBAssets.amaltheaWeaverCircusFortuneTeller
  , HBAssets.amaltheaWeaverAspirantOfCourage
  , HBAssets.amaltheaWeaverAspirantOfWisdom
  ]
    <> map fst amaltheaFinals

amaltheaFinals :: [(CardDef, Text)]
amaltheaFinals =
  [ (HBAssets.amaltheaWeaverOracleOfPurity, "OracleOfPurity")
  , (HBAssets.amaltheaWeaverOracleOfEnlightenment, "OracleOfEnlightenment")
  , (HBAssets.amaltheaWeaverOracleOfResolve, "OracleOfResolve")
  , (HBAssets.amaltheaWeaverOracleOfMystery, "OracleOfMystery")
  ]

deCultusBestiaeVersions :: [CardDef]
deCultusBestiaeVersions =
  [ HBAssets.deCultusBestiaeForgottenWorkOfApuleius
  , HBAssets.deCultusBestiaeInterpretationOfConviction
  , HBAssets.deCultusBestiaeInterpretationOfObsession
  ]
    <> map fst deCultusFinals

deCultusFinals :: [(CardDef, Text)]
deCultusFinals =
  [ (HBAssets.deCultusBestiaeProphecyOfTheBeyond, "ProphecyOfTheBeyond")
  , (HBAssets.deCultusBestiaeProphecyOfTheHorde, "ProphecyOfTheHorde")
  , (HBAssets.deCultusBestiaeProphecyOfTheEternal, "ProphecyOfTheEternal")
  , (HBAssets.deCultusBestiaeProphecyOfTheBehemoth, "ProphecyOfTheBehemoth")
  ]

-- Campaign store plumbing, mirroring the official campaigns' detection modules.

karmaTestKey, waxCancelsKey, waxReleasesKey, milkCancelsKey, shubDamageKey :: Text
karmaTestKey = "cemAchKarmaTest"
waxCancelsKey = "cemAchWaxCancels"
waxReleasesKey = "cemAchWaxReleases"
milkCancelsKey = "cemAchMilkCancels"
shubDamageKey = "cemAchShubDamage"

toweringAttackKey, traumaAtSetupKey :: Text
toweringAttackKey = "cemAchToweringAttack"
traumaAtSetupKey = "cemAchTraumaAtSetup"

lunaticKey :: InvestigatorId -> Text
lunaticKey iid = "cemAchMoonsReleased:" <> tshow iid

-- Priority so the write lands before the rest of the triggering message's
-- cascade, some of which clearQueue.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k
