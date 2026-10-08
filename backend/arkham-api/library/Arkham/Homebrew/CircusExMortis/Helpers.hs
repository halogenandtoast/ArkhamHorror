module Arkham.Homebrew.CircusExMortis.Helpers where

import Arkham.Ability (Ability, exists, forced, mkAbility, onlyOnce, restricted)
import Arkham.CampaignLogKey (recorded)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push, pushAll)
import Arkham.Classes.Query
import Arkham.Direction (Direction (..))
import Arkham.Distance (unDistance)
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Enemy.Types (Field (EnemyLocation, EnemyPlacement))
import Arkham.ForMovement (ForMovement (..))
import Arkham.GameEnv (getDistance, getRetiredInvestigators)
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.ChaosBag (getSealedChaosTokens)
import Arkham.Helpers.CustomChaosBag
import Arkham.Helpers.FlavorText (chaosTokenImg, cols, compose, img, p, setTitle, tokenReveal)
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Helpers.SkillTest (getIsBeingInvestigated, getSkillTestInvestigator)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Key (CircusExMortisKey (Destinies))
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Grid (Pos (..), positionColumn, positionRow)
import Arkham.Location.Types (Field (LocationPosition), LocationAttrs)
import Arkham.Matcher
import Arkham.Message (
  Message (AddToVictory, ReplaceCard),
  pattern PlaceCluesUpToClueValue,
  pattern RemoveLocation,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (getSomeRecordSetJSON, recordSetInsert, recordSetReplace, remember)
import Arkham.Message.Lifted.Move (enemyMoveTo, moveTo, moveToward)
import Arkham.Modifier (Modifier)
import Arkham.Name (Labeled (..), Named)
import Arkham.Name qualified as Name
import Arkham.Placement (Placement (InPosition))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioRemembered))
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.TokenBag
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Text qualified as T

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "circusExMortis" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

{- | "Each investigator suffers 1 physical or mental trauma and is defeated" -- how five
of the campaign's agendas resolve when their last stage advances. The two labels are the
campaign's own, not each agenda's, so this reads them from the campaign scope whatever
scope the caller is in. Resigned investigators are eliminated already, so
'eachInvestigator' skips them.
-}
sufferTraumaAndDefeat :: (ReverseQueue m, Sourceable source) => source -> m ()
sufferTraumaAndDefeat source = eachInvestigator \iid -> do
  chooseOneM iid $ campaignI18n $ scope "label" do
    labeled "physicalTrauma" $ sufferPhysicalTrauma iid 1
    labeled "mentalTrauma" $ sufferMentalTrauma iid 1
  investigatorDefeated source iid

-- * Moon tokens

-- | Moon tokens sealed on an investigator's investigator card (guide p1).
getSealedMoonTokens :: HasGame m => InvestigatorId -> m [ChaosToken]
getSealedMoonTokens iid =
  filter ((== MoonToken) . (.face)) <$> field InvestigatorSealedChaosTokens iid

{- | Every ☾ token sealed on a card, whichever card type it was sealed on --
"the number of ☾ tokens sealed on cards" (Dread of the New Moon). Sealed tokens
are not in the chaos bag, so this never overlaps 'moonToken' as selected.
-}
getAllSealedMoonTokens :: HasGame m => m [ChaosToken]
getAllSealedMoonTokens = filter ((== MoonToken) . (.face)) <$> getSealedChaosTokens

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
the persisted state. The temporary set-aside pile prevents repeats during the
Moon recursion.
-}
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

-- * Bacchanalia: vices

{- | The four vices an investigator may claim in the scenario's intro. Each one is
remembered about that investigator ('HomebrewScenarioLogKeyFor'), so the scenario
log both shows them and pays them out as bonus experience at the resolution.
-}
data Vice = Revelry | Intimacy | Opulence | Violence
  deriving stock (Show, Eq, Ord, Enum, Bounded)

allVices :: [Vice]
allVices = [minBound .. maxBound]

-- | Namespaced so the frontend can pick the campaign's i18n scope out of the key.
viceKey :: Vice -> Text
viceKey v = "circusExMortis.AViceFor" <> tshow v

viceByKey :: [(Text, Vice)]
viceByKey = [(viceKey v, v) | v <- allVices]

viceLogKey :: Named name => name -> InvestigatorId -> Vice -> ScenarioLogKey
viceLogKey name iid v = HomebrewScenarioLogKeyFor (viceKey v) (Name.labeled name iid)

recordVice :: ReverseQueue m => InvestigatorId -> Vice -> m ()
recordVice iid v = do
  name <- field InvestigatorName iid
  remember $ viceLogKey name iid v

vicesOf :: Set ScenarioLogKey -> InvestigatorId -> [Vice]
vicesOf ks iid =
  [ v | HomebrewScenarioLogKeyFor t (Labeled _ i) <- toList ks, i == iid, Just v <- [lookup t viceByKey]
  ]

{- | Reads the scenario log rather than modifiers, so it is safe to call from a
card's 'HasModifiersFor' — the shroud reductions on the four themed locations all
do. 'investigatorWithVice' is the matcher form.
-}
getVices :: HasGame m => InvestigatorId -> m [Vice]
getVices iid = flip vicesOf iid <$> scenarioField ScenarioRemembered

hasVice :: HasGame m => InvestigatorId -> Vice -> m Bool
hasVice iid v = elem v <$> getVices iid

getViceCount :: HasGame m => InvestigatorId -> m Int
getViceCount = fmap length . getVices

{- | Matcher form, backed by the 'ScenarioModifier's the scenario republishes from
the log (the three Cultists' @Prey@ lines need a matcher). Never read these from
inside a 'HasModifiersFor'; use 'hasVice' there.
-}
investigatorWithVice :: Vice -> InvestigatorMatcher
investigatorWithVice = InvestigatorWithModifier . ScenarioModifier . viceKey

{- | "While you are investigating <location>, if you have \"a vice for X,\" it gets
-2 shroud value." Banquet Hall, Statuary Gardens, Private Parlor and Collection Hall
each print this for a different vice. It reads the vice of whoever is running the
investigation, so only that investigator sees the reduced shroud.
-}
viceShroudReduction
  :: (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs -> Vice -> m ()
viceShroudReduction a v = modifySelfMaybe a do
  liftGuardM $ getIsBeingInvestigated a
  iid <- MaybeT getSkillTestInvestigator
  liftGuardM $ hasVice iid v
  pure [ShroudModifier (-2)]

{- | "You get +1 skill value while parleying at <location> until the end of the
round" — Banquet Hall, Statuary Gardens and Private Parlor all grant it. The
effect is enabled only for that investigator's parley tests while they are at the
location, and is removed at the end of the round.
-}
parleyBonusAt
  :: (ReverseQueue m, WithEffect m, Sourceable source)
  => source -> InvestigatorId -> LocationId -> m ()
parleyBonusAt source iid lid = effectWithSource source iid do
  enableOn
    $ EffectSkillTestMatchingWindow
    $ SkillTestMatches
      [ WhileParleying
      , SkillTestOfInvestigator (InvestigatorWithId iid <> InvestigatorAt (LocationWithId lid))
      ]
  removeOn EffectRoundWindow
  apply $ AnySkillValue 1

-- * Red Sunrise: rows

{- | "Rows" (guide p29): locations with the same name placed next to each other
horizontally. Red Sunrise places every location with 'placeInGrid', so a row is
simply a grid row. Above is nearer Ritual Clearing (higher y, since
'furyDirectionPos' already fixes north as @+y@), below nearer Forgotten Trail.
-}
rowOf :: (AsId l, IdOf l ~ LocationId) => l -> LocationMatcher
rowOf l = LocationInRowOf (LocationWithId $ asId l)

locationsInRowOf :: (HasGame m, AsId l, IdOf l ~ LocationId) => l -> m [LocationId]
locationsInRowOf = select . rowOf

-- | "X is the number of locations in your row."
getRowSize :: (HasGame m, AsId l, IdOf l ~ LocationId) => l -> m Int
getRowSize = selectCount . rowOf

getRowIndex :: (HasGame m, AsId l, IdOf l ~ LocationId) => l -> m (Maybe Int)
getRowIndex l = fmap positionRow <$> field LocationPosition (asId l)

-- | The row one step nearer Ritual Clearing.
rowAbove :: (HasGame m, AsId l, IdOf l ~ LocationId) => l -> m [LocationId]
rowAbove l = getRowIndex l >>= maybe (pure []) (select . LocationInRow . (+ 1))

-- | The row one step nearer Forgotten Trail.
rowBelow :: (HasGame m, AsId l, IdOf l ~ LocationId) => l -> m [LocationId]
rowBelow l = getRowIndex l >>= maybe (pure []) (select . LocationInRow . subtract 1)

{- | Order a row left to right. Path Forward names its column that way ("the
leftmost location in this row", "the second location from the right"), so the
ordering has to be by grid column rather than by whatever order 'select' returns.
-}
sortRowLeftToRight :: HasGame m => [LocationId] -> m [LocationId]
sortRowLeftToRight lids = do
  withColumns <- for lids \lid -> do
    mpos <- field LocationPosition lid
    pure (maybe 0 positionColumn mpos, lid)
  pure $ map snd $ sortOn fst withColumns

-- | Path Forward's column, counted from whichever end the card names.
data RowEnd = FromLeft Int | FromRight Int
  deriving stock (Show, Eq)

locationAtRowEnd :: HasGame m => RowEnd -> [LocationId] -> m (Maybe LocationId)
locationAtRowEnd end lids = do
  ordered <- sortRowLeftToRight lids
  pure $ case end of
    FromLeft n -> ordered !!? n
    FromRight n -> reverse ordered !!? n

{- | Written in Stone deals each investigator a destiny -- one of the eight words the
Fata Diana names -- and Thousand to One's cards ask for "the investigator whose destiny
is <word>" by it. The campaign log keeps them as a recorded set, one entry per
investigator, keyed by investigator id rather than by name: a name is neither unique
across a table nor stable across a replacement, and the cards have to find exactly one
seat. 'destinyEntry' is the only place the entry's shape is written down.
-}
destinyEntry :: InvestigatorId -> Text -> Text
destinyEntry iid word = unCardCode (toCardCode iid) <> ":" <> word

recordDestiny :: ReverseQueue m => InvestigatorId -> Text -> m ()
recordDestiny iid word = recordSetInsert Destinies [String $ destinyEntry iid word]

{- | Hand a destiny on to another seat.

The printed transfer is "if an investigator is killed or driven insane, their destiny is
transferred to the investigator chosen to replace them" (guide p19), and a player joining
mid-campaign in a departed investigator's place is the same move. Both ends go through
'destinyEntry', so the entry's shape stays written down in exactly one place.
-}
transferDestiny :: ReverseQueue m => InvestigatorId -> InvestigatorId -> m ()
transferDestiny oldIid newIid = do
  entries <- getSomeRecordSetJSON @Text Destinies
  for_ entries \entry ->
    for_ (destinyWordFor oldIid entry) \word ->
      recordSetReplace Destinies (recorded $ String entry) (recorded . String $ destinyEntry newIid word)

-- | The word in an entry, if the entry belongs to this investigator.
destinyWordFor :: InvestigatorId -> Text -> Maybe Text
destinyWordFor iid = T.stripPrefix (unCardCode (toCardCode iid) <> ":")

-- | Every destiny dealt, as (investigator, word).
getDestinies :: HasGame m => m [(InvestigatorId, Text)]
getDestinies = do
  entries <- getSomeRecordSetJSON @Text Destinies
  pure $ flip mapMaybe entries \entry -> case T.breakOn ":" entry of
    (code, rest) | Just word <- T.stripPrefix ":" rest -> Just (InvestigatorId (CardCode code), word)
    _ -> Nothing

-- | The destiny this investigator was dealt, if any.
destinyOf :: HasGame m => InvestigatorId -> m (Maybe Text)
destinyOf iid = lookup iid <$> getDestinies

{- | The destinies still keyed to an investigator who has left the table: retired (set
aside by the continue screen, so the roster can rejoin them), or killed or driven insane
and never replaced. A replacement rewrites the entry to its own id, so a destiny only
stays here while nobody has taken it over -- which makes these exactly the destinies a
player joining mid-campaign can claim.
-}
getDepartedDestinies :: HasGame m => m [(InvestigatorId, Text)]
getDepartedDestinies = do
  retired <- getRetiredInvestigators
  eliminated <- liftA2 (<>) (select KilledInvestigator) (select InsaneInvestigator)
  filter ((`elem` (retired <> eliminated)) . fst) <$> getDestinies

-- | Whether this investigator's destiny is the given word.
hasDestiny :: HasGame m => InvestigatorId -> Text -> m Bool
hasDestiny iid word = (== Just word) <$> destinyOf iid

{- | The seat a destiny was dealt to, as a matcher. A game deals one destiny per
investigator, so most of the eight words belong to nobody at the table and this is
vacuous for them -- which is what the cards want: an ability gated on a destiny nobody
holds can never be used.
-}
investigatorWithDestiny :: HasGame m => Text -> m InvestigatorMatcher
investigatorWithDestiny word = do
  destinies <- getDestinies
  pure $ mapOneOf InvestigatorWithId [iid | (iid, w) <- destinies, w == word]

-- | The 'ScenarioModifier' tag Thousand to One republishes a destiny under.
destinyKey :: Text -> Text
destinyKey word = "destiny." <> word

{- | Pure matcher form of 'investigatorWithDestiny', backed by the 'ScenarioModifier's
Thousand to One republishes from the campaign log -- the same seam the Bacchanalia vices
use, and needed for the same reason: 'getAbilities' is pure, so an ability whose criteria
or window names a destiny cannot read the log itself. Use 'hasDestiny' /
'investigatorWithDestiny' inside a 'HasModifiersFor'; use this one in 'getAbilities'.
-}
investigatorWithDestinyModifier :: Text -> InvestigatorMatcher
investigatorWithDestinyModifier = InvestigatorWithModifier . ScenarioModifier . destinyKey

{- | "Flip it and move it to the victory display", as each Destiny story card's non-story
face says. Act 1 counts Destiny /story/ cards in the victory display, so the enemy (or
location, or asset) face has to be swapped back for the story one before the card lands
there.

The recode has to come /last/, because it is the only half that cannot be aimed at the
entity. 'EnemyAttrs' answers 'isTarget' for its own @CardIdTarget@, so the enemy claims
@AddToVictory miid (CardIdTarget attrs.cardId)@ and runs the whole enemy victory pipeline
with @field EnemyCard@ -- rebuilt from @enemyOriginalCardCode@, so blind to a recode that
already happened -- and the enemy face is what lands in the display. Recoding afterwards
instead rides the @ReplaceCard@ clause in @Scenario/Runner@, whose whole job is keeping a
card flipped to its other side in sync wherever a scenario zone is holding it. Running the
pipeline rather than dodging it is also what keeps the enemy's leave-play windows and its
attached cards' cleanup, which a bare removal skips (#5309).

@removeEntity@ is the caller's own removal message so this serves every face the Destiny
stories wear: @RemoveEnemy@, @RemoveLocation@, @RemoveAsset@. Only the enemy has a
pipeline to claim the target; a location or asset is filed straight from the card map, and
the recode behind it corrects that entry just the same.
-}
flipToVictoryDisplay
  :: ReverseQueue m => Maybe InvestigatorId -> CardDef -> CardId -> Message -> m ()
flipToVictoryDisplay miid storyDef cardId removeEntity =
  pushAll
    [ removeEntity
    , AddToVictory miid (CardIdTarget cardId)
    , ReplaceCard cardId (lookupCard storyDef.cardCode cardId)
    ]

{- | Candidates for "move the nearest enemy once toward <location>", shared by the
scenario reference card's elder thing token and Shadowed Wilderness (:173).

Returns every enemy tied at the fewest moves from reaching the location, among those
that can be moved and are not already standing there. A pushed 'MoveToward' cannot be
observed afterwards, so :173 has to know *before* pushing whether anything will move,
for its "if no enemy moves" clause. The caller decides whether one is chosen or all of
them move.

Distance is measured from the ENEMY to the location rather than the other way round,
which is why this does not use 'NearestEnemyToLocation'. That matcher measures the other
way, out of the location towards the enemy, and Red Sunrise's rows connect one way only
-- downward -- so every enemy above you answers "no path", they all tie at no distance
at all, and its Fallback then offers the whole board. The enemy's own journey is the one
the card means, and the one that exists.
-}
nearestEnemiesAbleToMoveToward :: HasGame m => LocationId -> EnemyMatcher -> m [EnemyId]
nearestEnemiesAbleToMoveToward lid matcher = do
  candidates <- select $ matcher <> EnemyCanMove <> not_ (EnemyAt $ LocationWithId lid)
  withDistances <- forMaybeM candidates \eid -> runMaybeT do
    elid <- MaybeT $ field EnemyLocation eid
    distance <- MaybeT $ getDistance elid lid
    pure (eid, unDistance distance)
  pure case sortOn snd withDistances of
    [] -> []
    nearest@((_, fewest) : _) -> [eid | (eid, distance) <- nearest, distance == fewest]

-- * Thousand to One

{- | "After Shub-Niggurath leaves <this location>", the Forced ability Primal Forest and
High Thicket both print on either face. 'Window.EnemyLeaves' is raised behind the move's
own @Do (EnemyMove)@, so Shub-Niggurath has already arrived at its destination by the
time this resolves -- which is what makes "each OTHER enemy at <this location>" simply
"every enemy still standing here".
-}
shubNiggurathLeaves :: Int -> LocationAttrs -> Ability
shubNiggurathLeaves n a =
  onlyOnce
    $ mkAbility a n
    $ forced
    $ EnemyLeaves #after (be a) (enemyIs Enemies.shubNiggurath)

{- | "Move each investigator and other enemy at <this location> once toward Silent
Clearing." Shared by Primal Forest and High Thicket, which differ only in where the
location itself goes afterwards.
-}
scatterTowardSilentClearing :: ReverseQueue m => LocationAttrs -> m ()
scatterTowardSilentClearing a = do
  let silentClearing = locationIs Locations.silentClearing
  selectEach (investigatorAt a) (`moveToward` silentClearing)
  selectEach (enemyAt a <> not_ (enemyIs Enemies.shubNiggurath)) (`moveToward` silentClearing)

{- | "Move each investigator and enemy on it to a connecting location, flip it, and move
it to the victory display" -- the Forced that Forest Chasm, Canyon Entrance, Marked Grove
and Defiled Woods all print once their task is done.

Each investigator chooses their own destination and the lead chooses for the enemies, the
split Waterfront Warehouse (Dawn/Dusk) already uses for this wording. Investigator
destinations come from 'getConnectedMoveLocations', so a connection they cannot use is
never offered; enemies are not bound by investigator movement restrictions and read the
connections directly.

The removal is a bare 'RemoveLocation', which is what @AddToVictory@ on a location target
pushes for itself -- the clue/damage/horror/resource the location was holding goes with it.
-}
destinyLocationOvercome
  :: (ReverseQueue m, Sourceable source)
  => source -> InvestigatorId -> CardDef -> LocationAttrs -> m ()
destinyLocationOvercome source lead storyDef a = do
  selectEach (investigatorAt a) \iid -> do
    destinations <- getConnectedMoveLocations iid source
    chooseTargetM iid destinations $ moveTo source iid
  connected <- select $ ConnectedTo ForMovement (be a)
  selectEach (enemyAt a) $ chooseTargetM lead connected . enemyMoveTo source
  flipToVictoryDisplay Nothing storyDef a.cardId (RemoveLocation a.id)
