module Arkham.Homebrew.CircusExMortis.Helpers where

import Arkham.Ability (Ability, exists, forced, restricted)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Direction (Direction (..))
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.CustomChaosBag
import Arkham.Helpers.FlavorText (chaosTokenImg, cols, compose, img, p, setTitle, tokenReveal)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (scenarioField, setScenarioMeta)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message (pattern PlaceCluesUpToClueValue)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Placement (Placement (InPosition))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Source
import Arkham.Target
import Arkham.TokenBag
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

lookupRatsInACage :: HasCardDef a => a -> Maybe (CardDef, ChaosTokenFace)
lookupRatsInACage (toCardDef -> def) = lookup def (toList ratsInACageVariants)

bigTopRings :: LocationMatcher
bigTopRings = LocationWithTitle "The Big Top"

-- * Story-asset versions (Amalthea Weaver / De Cultus Bestiae)

-- | Find the owner and current version of a versioned story asset.
findVersionOwner
  :: HasGame m => [CardDef] -> m (Maybe (InvestigatorId, CardDef))
findVersionOwner defs =
  listToMaybe . catMaybes <$> for defs \def -> fmap (,def) <$> getOwner def

-- | Base version first; 'findVersionOwner' returns whichever one is owned.
getAmaltheaWeaverOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
getAmaltheaWeaverOwner =
  findVersionOwner
    [ Assets.amaltheaWeaverCircusFortuneTeller
    , Assets.amaltheaWeaverAspirantOfCourage
    , Assets.amaltheaWeaverAspirantOfWisdom
    , Assets.amaltheaWeaverOracleOfPurity
    , Assets.amaltheaWeaverOracleOfResolve
    , Assets.amaltheaWeaverOracleOfEnlightenment
    , Assets.amaltheaWeaverOracleOfMystery
    ]

getDeCultusBestiaeOwner :: HasGame m => m (Maybe (InvestigatorId, CardDef))
getDeCultusBestiaeOwner =
  findVersionOwner
    [ Assets.deCultusBestiaeForgottenWorkOfApuleius
    , Assets.deCultusBestiaeInterpretationOfConviction
    , Assets.deCultusBestiaeInterpretationOfObsession
    , Assets.deCultusBestiaeProphecyOfTheBeyond
    , Assets.deCultusBestiaeProphecyOfTheEternal
    , Assets.deCultusBestiaeProphecyOfTheHorde
    , Assets.deCultusBestiaeProphecyOfTheBehemoth
    ]

-- * Curse of the Rougarou side story

curseOfTheRougarouId :: ScenarioId
curseOfTheRougarouId = "81001"

-- * Harm's Way: the fury bag

{- | The fury bag (guide p11) is a second bag of tokens that are explicitly NOT
chaos tokens: it is never drawn from during a skill test and has no
'HasChaosTokenValue'. 'ChaosTokenFace' is reused purely as the tagged union of
faces the bag can hold. It is the scenario's named "fury" custom chaos bag,
so every card that says "reveal a fury token" shares its state and debug override.
-}
furyBagKey :: Text
furyBagKey = "fury"

getFuryBag :: HasGame m => m CustomChaosBag
getFuryBag = getCustomChaosBag furyBagKey

setFuryBag :: ReverseQueue m => CustomChaosBag -> m ()
setFuryBag = setCustomChaosBag furyBagKey

{- | Write one key of the scenario's meta object, leaving the rest alone.
'setScenarioMeta' replaces the whole value, and the engine has no per-key
setter, so this reads the current object first — which means one call per
handler: two would both read the pre-write value.
-}
setScenarioMetaKey :: (ReverseQueue m, ToJSON a) => Key -> a -> m ()
setScenarioMetaKey k v = do
  meta <- scenarioField ScenarioMeta
  let object' = case meta of
        Object o -> o
        _ -> KeyMap.empty
  setScenarioMeta $ Object $ KeyMap.insert k (toJSON v) object'

{- | "Add a ☾ token to the fury bag" (Restless Night, Midnight Snacking). The
bag only ever grows, so this is the one place its contents change.
-}
addFuryToken :: ReverseQueue m => ChaosTokenFace -> m ()
addFuryToken face = do
  bag <- getFuryBag
  tokenId <- getRandom
  setFuryBag bag {bagTokens = BagToken tokenId face : bag.tokens}

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

-- | Fury directions are relative to each Dark Young, not the map's center.
furyAttackPosition :: Pos -> FuryDirection -> Pos
furyAttackPosition (Pos x y) direction =
  let Pos dx dy = furyDirectionPos direction
   in Pos (x + dx) (y + dy)

-- | Grid position one step further out, where Camp Outskirts is placed.
furyDirectionOutwardPos :: FuryDirection -> Pos
furyDirectionOutwardPos = \case
  FuryNorth -> Pos 0 2
  FurySouth -> Pos 0 (-2)
  FuryWest -> Pos (-2) 0
  FuryEast -> Pos 2 0

{- | Draw @n@ pending tokens without replacement; a ☾ costs nothing but adds two
more pending draws (The Dark Young Stir's recursion). Every drawn token is
returned once the instruction resolves; only a consumed debug override changes
the persisted state.
-}

-- The temporary set-aside pile prevents repeats during Moon recursion.
drawFuryBagTokens
  :: MonadRandom m => CustomChaosBag -> Int -> m ([ChaosTokenFace], CustomChaosBag)
drawFuryBagTokens bag n
  | n <= 0 = pure ([], bag)
  | otherwise = do
      (drawn, bag') <- drawBagToken (.face) bag
      case drawn of
        Nothing -> pure ([], bag')
        Just token -> do
          let face = token.face
          let pending = if face == MoonToken then n + 1 else n - 1
          (faces, finalBag) <- drawFuryBagTokens (setAsideBagToken bag') pending
          pure (face : faces, finalBag)

{- | "Reveal a fury token", resolved through The Dark Young Stir...: every
Towering Dark Young in play immediately attacks each investigator at the
location the drawn token names. A ☾ reveals two more tokens instead.
-}
revealFuryToken :: (ReverseQueue m, Sourceable source) => source -> m ()
revealFuryToken source = do
  bag <- getFuryBag
  (faces, drawnBag) <- drawFuryBagTokens bag 1
  setFuryBag $ returnSetAsideTokens drawnBag
  for_ faces \face -> scenarioI18n "harmsWay" $ scope "furyReveal" do
    case furyDirection face of
      Nothing -> storyWithContinue $ tokenReveal do
        setTitle "title"
        cols do
          img Stories.theDarkYoungStir
          compose do
            chaosTokenImg face
            p "moon"
      Just direction -> do
        darkYoung <- select $ EnemyWithTitle "Towering Dark Young"
        storyWithContinue $ tokenReveal do
          setTitle "title"
          cols do
            img Stories.theDarkYoungStir
            compose do
              chaosTokenImg face
              p $ case direction of
                FuryNorth -> "north"
                FurySouth -> "south"
                FuryWest -> "west"
                FuryEast -> "east"
        -- Preserve single-target attacks so enemy attack reactions still match.
        for_ darkYoung \eid -> do
          placement <- field EnemyPlacement eid
          case placement of
            InPosition pos -> do
              let targetPos = furyAttackPosition pos direction
              -- Outskirts also counts as the Camp location on its side of the map.
              -- Camp Outskirts counts as the Camp location on its side of the map, so
              -- the outward grid slot aliases onto the same direction.
              locations <- case find ((== targetPos) . furyDirectionPos) [FuryNorth, FurySouth, FuryWest, FuryEast] of
                Just side ->
                  catMaybes
                    <$> traverse
                      (selectOne . LocationInPosition)
                      [furyDirectionPos side, furyDirectionOutwardPos side]
                Nothing -> select $ LocationInPosition targetPos
              investigators <- concatMapM (select . InvestigatorAt . LocationWithId) locations
              for_ investigators $ initiateEnemyAttack eid source
            _ -> pure ()

-- * The Primrose Path

moonlitForests :: LocationMatcher
moonlitForests = LocationWithTitle "Moonlit Forest"

{- | Both Primrose Path agenda fronts read "Adjacent copies of Moonlit Forest are
connected to each other." Adjacency is the scenario grid; every other connection in
the scenario comes from the printed connection symbols.
-}
adjacentMoonlitForestConnection :: LocationId -> ModifierType
adjacentMoonlitForestConnection lid =
  ConnectedToWhen (LocationWithId lid)
    $ LocationMatchAny [LocationInDirection d (LocationWithId lid) | d <- [minBound .. maxBound]]
    <> moonlitForests

{- | Remote Cabin and Woodland Overlook each sit at one end of the grid, connected to
the whole column of three Moonlit Forest copies beside them (guide p7). Reading the
column off whichever side has a neighbour keeps this independent of which end each
location was placed at.
-}
neighbouringMoonlitForestColumn :: LocationMatcher -> LocationMatcher
neighbouringMoonlitForestColumn self =
  moonlitForests
    <> LocationInColumnOf (LocationMatchAny [LocationInDirection d self | d <- [LeftOf, RightOf]])

-- * Piper at the Gates of Dawn

{- | The Forced ability all three agenda fronts print. 'LocationNotAtClueLimit' keeps it
from firing when every revealed ring is already at (or over) its printed clue value,
which is the common case once act 1's back has added its extra clues.
-}
replenishBigTopRingsAbility :: (HasCardCode a, Sourceable a) => a -> Ability
replenishBigTopRingsAbility a =
  restricted a 1 (exists replenishableBigTopRings) $ forced $ RoundEnds #when

replenishableBigTopRings :: LocationMatcher
replenishableBigTopRings = bigTopRings <> RevealedLocation <> LocationNotAtClueLimit

{- | "Replenish all clues on each revealed The Big Top location": the message tops each
ring up to its printed reveal value, so the extra clues act 1's back placed on the rings
are not restored.
-}
replenishBigTopRings :: (ReverseQueue m, Sourceable source) => source -> m ()
replenishBigTopRings source = do
  n <- getPlayerCount
  selectEach replenishableBigTopRings \lid ->
    push $ PlaceCluesUpToClueValue lid (toSource source) n

{- | Agendas 1b and 2b: each investigator without a ☾ token sealed on their investigator
card searches the chaos bag for one and seals it there, and loses an action if they
cannot. Resolve one investigator at a time: each seal takes a ☾ out of the bag, so the
next investigator has to read the bag as it now stands.
-}
sealMoonTokenOrLoseAction
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
sealMoonTokenOrLoseAction source iid = unlessM (iid <=~> hasSealedMoonToken) do
  moonInBag <- selectAny moonToken
  if moonInBag then sealMoonTokenOn iid else loseActions iid source 1
