module Arkham.Homebrew.CircusExMortis.Helpers where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Campaign (getCompletedSteps, getOwner)
import Arkham.Helpers.Scenario (getScenarioMetaKeyDefault, scenarioField, setScenarioMeta)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message (ShuffleIn (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Source
import Arkham.Target
import Data.Aeson.KeyMap qualified as KeyMap

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "circusExMortis" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

-- * Moon tokens

-- | Moon tokens sealed on an investigator's investigator card (guide p1).
getSealedMoonTokens :: HasGame m => InvestigatorId -> m [ChaosToken]
getSealedMoonTokens iid =
  filter ((== MoonToken) . (.face)) <$> field InvestigatorSealedChaosTokens iid

moonToken :: ChaosTokenMatcher
moonToken = ChaosTokenFaceIs MoonToken

{- | The ☾ token's printed modifier is 0, but 'NoModifier' is inert: effects that
reduce a token's modifier (Primordial Evils) would slide off it.
-}
moonTokenValue :: ChaosTokenValue
moonTokenValue = ChaosTokenValue MoonToken (NegativeModifier 0)

hasSealedMoonToken :: InvestigatorMatcher
hasSealedMoonToken = InvestigatorWithSealedChaosToken moonToken

{- | Tokens sealed on cards at a location. Seals land on investigator cards (the
☾ reveal effect) and on assets (De Cultus Bestiae), so those are the two pools
"sealed on cards at your location" can draw from.
-}
getSealedTokensAtMatching :: HasGame m => ChaosTokenMatcher -> LocationId -> m [ChaosToken]
getSealedTokensAtMatching matcher lid = do
  onInvestigators <- select $ SealedOnInvestigator (InvestigatorAt $ LocationWithId lid) matcher
  onAssets <- select $ SealedOnAsset (AssetAtLocation lid) matcher
  pure $ nub (onInvestigators <> onAssets)

-- | Moon tokens sealed on cards at a location.
getSealedMoonTokensAt :: HasGame m => LocationId -> m [ChaosToken]
getSealedMoonTokensAt = getSealedTokensAtMatching moonToken

{- | Any sealed token on a card at a location: Amalthea's release riders say "a
token", not "a ☾ token", and are read literally.
-}
getSealedTokensAt :: HasGame m => LocationId -> m [ChaosToken]
getSealedTokensAt = getSealedTokensAtMatching AnyChaosToken

-- | Moon tokens sealed on the cards an investigator controls.
getSealedMoonTokensControlledBy :: HasGame m => InvestigatorId -> m [ChaosToken]
getSealedMoonTokensControlledBy iid = do
  own <- select $ SealedOnInvestigator (InvestigatorWithId iid) moonToken
  onAssets <- select $ SealedOnAsset (assetControlledBy iid) moonToken
  pure $ nub (own <> onAssets)

-- | "Search the chaos bag for a ☾ token and seal it on your investigator card."
sealMoonTokenOn :: ReverseQueue m => InvestigatorId -> m ()
sealMoonTokenOn iid = sealMoonTokenOnTarget iid iid

-- | "Search the chaos bag for a ☾ token and seal it on <target>."
sealMoonTokenOnTarget :: (ReverseQueue m, Targetable target) => InvestigatorId -> target -> m ()
sealMoonTokenOnTarget iid target = selectOne moonToken >>= traverse_ (sealChaosToken iid target)

-- | Release a sealed token: it returns to the chaos bag.
releaseToken :: ReverseQueue m => ChaosToken -> m ()
releaseToken = unsealChaosToken

-- | Release a sealed moon token.
releaseMoonToken :: ReverseQueue m => ChaosToken -> m ()
releaseMoonToken = releaseToken

{- | The "release a ☾ token sealed on your investigator card" ability shared by
'Smoke and Mirrors' and 'Out and Away'.
-}
releaseAMoonToken :: ReverseQueue m => InvestigatorId -> m ()
releaseAMoonToken iid = chooseReleaseToken iid =<< getSealedMoonTokens iid

-- | Pick one of @tokens@ to release.
chooseReleaseToken :: ReverseQueue m => InvestigatorId -> [ChaosToken] -> m ()
chooseReleaseToken iid tokens =
  chooseOneM iid $ for_ tokens \token ->
    targeting (ChaosTokenTarget token) $ releaseToken token

-- | "Release up to @n@ tokens": the Done button covers the optional "may".
chooseReleaseTokens :: ReverseQueue m => InvestigatorId -> Int -> [ChaosToken] -> m ()
chooseReleaseTokens iid n tokens = unless (null tokens) do
  chooseUpToNM_ iid n $ for_ tokens \token ->
    targeting (ChaosTokenTarget token) $ releaseToken token

-- * One Night Only

{- | Each 'Rats in a Cage' hides the Illusory Locus at a different location and
permanently adds a different chaos token when it advances. Resolution 1 reads
the token back off whichever variant was chosen for the scenario.
-}
ratsInACageVariants :: NonEmpty (CardDef, (CardDef, ChaosTokenFace))
ratsInACageVariants =
  (Acts.ratsInACage_005, (Locations.animalCages, Tablet))
    :| [ (Acts.ratsInACage_006, (Locations.carousel, Tablet))
       , (Acts.ratsInACage_007, (Locations.gamesGallery, Cultist))
       , (Acts.ratsInACage_008, (Locations.performerTrailers, Cultist))
       ]

lookupRatsInACage :: CardDef -> Maybe (CardDef, ChaosTokenFace)
lookupRatsInACage def = lookup def (toList ratsInACageVariants)

-- * Story-asset versions (Amalthea Weaver / De Cultus Bestiae)

-- | Every printing of Amalthea Weaver, base version first.
amaltheaWeaverVersions :: [CardDef]
amaltheaWeaverVersions =
  [ Assets.amaltheaWeaverCircusFortuneTeller
  , Assets.amaltheaWeaverAspirantOfCourage
  , Assets.amaltheaWeaverAspirantOfWisdom
  , Assets.amaltheaWeaverOracleOfPurity
  , Assets.amaltheaWeaverOracleOfResolve
  , Assets.amaltheaWeaverOracleOfEnlightenment
  , Assets.amaltheaWeaverOracleOfMystery
  ]

-- | Every printing of De Cultus Bestiae, base version first.
deCultusBestiaeVersions :: [CardDef]
deCultusBestiaeVersions =
  [ Assets.deCultusBestiaeForgottenWorkOfApuleius
  , Assets.deCultusBestiaeInterpretationOfConviction
  , Assets.deCultusBestiaeInterpretationOfObsession
  , Assets.deCultusBestiaeProphecyOfTheBeyond
  , Assets.deCultusBestiaeProphecyOfTheEternal
  , Assets.deCultusBestiaeProphecyOfTheHorde
  , Assets.deCultusBestiaeProphecyOfTheBehemoth
  ]

-- | Find the owner and current version of a versioned story asset.
findVersionOwner
  :: HasGame m => [CardDef] -> m (Maybe (InvestigatorId, CardDef))
findVersionOwner defs =
  listToMaybe . catMaybes <$> for defs \def -> fmap (,def) <$> getOwner def

getAmaltheaWeaverOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
getAmaltheaWeaverOwner = findVersionOwner amaltheaWeaverVersions

getDeCultusBestiaeOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
getDeCultusBestiaeOwner = findVersionOwner deCultusBestiaeVersions

{- | Swap a versioned campaign story card for its next version in the same
investigator's deck (Relic of Ages pattern: remove the old def, add the new
one without counting toward deck size). No-op when nobody owns the old
version.
-}
swapCampaignCard :: ReverseQueue m => CardDef -> CardDef -> m ()
swapCampaignCard old new =
  getOwner old >>= traverse_ \iid -> do
    removeCampaignCard old
    addCampaignCardToDeck iid DoNotShuffleIn new

-- * Curse of the Rougarou side story

{- | The guide offers Curse of the Rougarou between Harm's Way and All Points
West; All Points West reads its Back on Track intro when the side story was
the most recently completed scenario. Completion itself is recorded by the
official scenario's resolutions (the TheRougarou* campaign log keys).
-}
playedCurseOfTheRougarouEnRoute :: HasGame m => m Bool
playedCurseOfTheRougarouEnRoute = do
  steps <- getCompletedSteps
  -- completed steps are stored most-recent-first
  pure $ case mapMaybe (.scenario) steps of
    (sid : _) -> sid == curseOfTheRougarouId
    _ -> False

curseOfTheRougarouId :: ScenarioId
curseOfTheRougarouId = "81001"

-- * Harm's Way: the fury bag

{- | The fury bag (guide p11) is a second bag of tokens that are explicitly NOT
chaos tokens: it is never drawn from during a skill test and has no
'HasChaosTokenValue'. 'ChaosTokenFace' is reused purely as the tagged union of
faces the bag can hold. The bag lives in scenario meta so every card that says
"reveal a fury token" reads the same list.
-}
furyBagKey :: Key
furyBagKey = "furyBag"

-- | The bag the scenario is set up with; agenda flips add ☾ tokens on top.
initialFuryBag :: [ChaosTokenFace]
initialFuryBag = [Skull, Cultist, Tablet, ElderThing]

getFuryBag :: HasGame m => m [ChaosTokenFace]
getFuryBag = getScenarioMetaKeyDefault furyBagKey initialFuryBag

setFuryBag :: ReverseQueue m => [ChaosTokenFace] -> m ()
setFuryBag bag = do
  meta <- scenarioField ScenarioMeta
  let object' = case meta of
        Object o -> o
        _ -> KeyMap.empty
  setScenarioMeta $ Object $ KeyMap.insert furyBagKey (toJSON bag) object'

-- | Setup: "Create a separate bag consisting of a ☠, ☾, 𝍎, and ✷ token."
initFuryBag :: ReverseQueue m => m ()
initFuryBag = setFuryBag initialFuryBag

{- | "Add a ☾ token to the fury bag" (Restless Night, Midnight Snacking). The
bag only ever grows, so this is the one place its contents change.
-}
addFuryToken :: ReverseQueue m => ChaosTokenFace -> m ()
addFuryToken face = do
  bag <- getFuryBag
  setFuryBag (face : bag)

{- | The direction vocabulary shared by The Dark Young Stir... and Act 1's back.
It is a fixed mapping onto the four Camp locations flanking Ringmaster's
Trailer, not something computed per Towering Dark Young (which have no
location); Act 1's worked example — "the ☠ token would place the location above
the top copy of Crowded Row" — is what pins it down.
-}
data FuryDirection = FuryNorth | FurySouth | FuryWest | FuryEast
  deriving stock (Show, Eq)

furyDirection :: ChaosTokenFace -> Maybe FuryDirection
furyDirection = \case
  Skull -> Just FuryNorth
  Cultist -> Just FurySouth
  Tablet -> Just FuryWest
  ElderThing -> Just FuryEast
  _ -> Nothing

-- | Grid position of the Camp location a direction names.
furyDirectionPos :: FuryDirection -> Pos
furyDirectionPos = \case
  FuryNorth -> Pos 0 1
  FurySouth -> Pos 0 (-1)
  FuryWest -> Pos (-1) 0
  FuryEast -> Pos 1 0

-- | Grid position one step further out, where Camp Outskirts is placed.
furyDirectionOutwardPos :: FuryDirection -> Pos
furyDirectionOutwardPos = \case
  FuryNorth -> Pos 0 2
  FurySouth -> Pos 0 (-2)
  FuryWest -> Pos (-2) 0
  FuryEast -> Pos 2 0

furyDirectionLocation :: HasGame m => FuryDirection -> m (Maybe LocationId)
furyDirectionLocation = selectOne . LocationInPosition . furyDirectionPos

{- | Every location a direction resolves to. Camp Outskirts is "considered to be
at the same position as the adjacent [[Camp]] location while resolving fury
tokens", and Act 1 places it one step further out along the direction it drew,
so the outward grid slot aliases onto the same direction.
-}
furyDirectionLocations :: HasGame m => FuryDirection -> m [LocationId]
furyDirectionLocations direction =
  catMaybes
    <$> traverse
      (selectOne . LocationInPosition)
      [furyDirectionPos direction, furyDirectionOutwardPos direction]

{- | Draw @n@ pending tokens without replacement; a ☾ costs nothing but adds two
more pending draws (The Dark Young Stir's recursion). The bag itself is never
written to: every drawn token is returned once the instruction resolves, so the
net change is zero.
-}
drawFuryTokens :: MonadRandom m => [ChaosTokenFace] -> Int -> m [ChaosTokenFace]
drawFuryTokens pool n
  | n <= 0 = pure []
  | otherwise = case nonEmpty pool of
      Nothing -> pure []
      Just candidates -> do
        face <- sample candidates
        let pending = if face == MoonToken then n + 1 else n - 1
        (face :) <$> drawFuryTokens (deleteFirst face pool) pending

{- | "Reveal a fury token", resolved through The Dark Young Stir...: every
Towering Dark Young in play immediately attacks each investigator at the
location the drawn token names. A ☾ reveals two more tokens instead.
-}
revealFuryToken :: (ReverseQueue m, Sourceable source) => source -> m ()
revealFuryToken source = do
  bag <- getFuryBag
  faces <- drawFuryTokens bag 1
  for_ (mapMaybe furyDirection faces) \direction -> do
    darkYoung <- select $ EnemyWithTitle "Towering Dark Young"
    locations <- furyDirectionLocations direction
    for_ locations \lid -> do
      investigators <- select $ InvestigatorAt (LocationWithId lid)
      -- One real single-target attack per pair: the Towering Dark Young
      -- reactions hang off EnemyWouldAttack and Cautious Jailers off
      -- EnemyAttacksEvenIfCancelled, and both noMatch on massive multi-target
      -- attacks.
      for_ darkYoung \eid -> for_ investigators $ initiateEnemyAttack eid source

{- | Act 1's back reads the same direction table for a different purpose: a ☾ is
ignored and another token drawn (no recursion), and nothing attacks.
-}
drawFuryTokenForDirection :: (HasGame m, MonadRandom m) => m (Maybe FuryDirection)
drawFuryTokenForDirection = go =<< getFuryBag
 where
  go pool = case nonEmpty pool of
    Nothing -> pure Nothing
    Just candidates -> do
      face <- sample candidates
      case furyDirection face of
        Just direction -> pure (Just direction)
        Nothing -> go (deleteFirst face pool)
