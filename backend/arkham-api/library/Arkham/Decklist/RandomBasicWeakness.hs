module Arkham.Decklist.RandomBasicWeakness (
  module Arkham.Decklist.RandomBasicWeakness,
  module Arkham.Decklist.CardPool,
) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Decklist.CardPool
import Arkham.Decklist.Type
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Taboo
import Data.List.Extra (groupSortOn)
import Data.Text qualified as T

data RandomBasicWeaknessContext = RandomBasicWeaknessContext
  { rbwInvestigatorClass :: ClassSymbol
  , rbwPlayerCount :: Int
  , rbwTaboo :: Maybe TabooList
  , rbwCardPool :: Maybe ArkhamBuildCardPool
  , rbwStandalone :: Bool
  }
  deriving stock (Show, Eq, Ord)

{- | The context deck building uses. Taboo and card pool are the only things the decklist
contributes, so anything drawing a weakness mid-game supplies them directly instead.
-}
decklistWeaknessContext
  :: ClassSymbol -> Int -> Bool -> Maybe ArkhamDBDecklist -> RandomBasicWeaknessContext
decklistWeaknessContext investigatorClass playerCount standalone mDecklist =
  RandomBasicWeaknessContext
    { rbwInvestigatorClass = investigatorClass
    , rbwPlayerCount = playerCount
    , rbwTaboo = mDecklist >>= fromTabooId . taboo_id
    , rbwCardPool = mDecklist >>= parseArkhamBuildCardPool
    , rbwStandalone = standalone
    }

newtype ArkhamBuildDecklistMeta = ArkhamBuildDecklistMeta
  { card_pool :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON ArkhamBuildDecklistMeta where
  parseJSON = withObject "ArkhamBuildDecklistMeta" $ \o -> do
    card_pool <- o .:? "card_pool"
    pure $ ArkhamBuildDecklistMeta {..}

parseArkhamBuildCardPool :: ArkhamDBDecklist -> Maybe ArkhamBuildCardPool
parseArkhamBuildCardPool decklist = do
  metaText <- meta decklist
  ArkhamBuildDecklistMeta {card_pool} <- decode (encodeUtf8 $ fromStrict metaText)
  poolText <- card_pool
  let tokens = filter (not . T.null) $ T.strip <$> T.splitOn "," poolText
  guard (notNull tokens)
  pure $ ArkhamBuildCardPool tokens

randomBasicWeaknessCandidates :: RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessCandidates = randomBasicWeaknessCandidatesMatching (const True)

{- | As 'randomBasicWeaknessCandidates', but with an extra filter layered on top of the
built-in ones -- the caller's 'Arkham.Matcher.CardMatcher' for an in-game search, say.
-}
randomBasicWeaknessCandidatesMatching
  :: (CardDef -> Bool) -> RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessCandidatesMatching f ctx =
  filter (\cardDef -> weaknessFilter ctx cardDef && f cardDef)
    $ tabooMutate ctx
    <$> allBasicWeaknesses

randomBasicWeaknessSamplingCandidates :: RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessSamplingCandidates = randomBasicWeaknessSamplingCandidatesMatching (const True)

randomBasicWeaknessSamplingCandidatesMatching
  :: (CardDef -> Bool) -> RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessSamplingCandidatesMatching f ctx =
  let candidates = randomBasicWeaknessCandidatesMatching f ctx
   in if null candidates then randomBasicWeaknessCandidatesIgnoringCardPool f ctx else candidates

{- | The legal candidates grouped by the card they print, so a weakness reprinted in
Revised Core or Chapter 2 forms one group rather than two or three entries.
-}
randomBasicWeaknessSamplingGroups :: RandomBasicWeaknessContext -> [NonEmpty CardDef]
randomBasicWeaknessSamplingGroups = randomBasicWeaknessSamplingGroupsMatching (const True)

randomBasicWeaknessSamplingGroupsMatching
  :: (CardDef -> Bool) -> RandomBasicWeaknessContext -> [NonEmpty CardDef]
randomBasicWeaknessSamplingGroupsMatching f =
  mapMaybe nonEmpty . groupSortOn canonicalCardCode . randomBasicWeaknessSamplingCandidatesMatching f

{- | Sample the weakness first, then the printing. Sampling the flat candidate list
instead would weight a reprinted weakness two or three times as heavily as one that was
only ever printed once, while grouping the pool up front would drop the only legal
printing for a card pool restricted to Revised Core or Chapter 2. Printings differ only
in art, so the second sample keeps that variety.
-}
sampleRandomBasicWeakness :: MonadRandom m => RandomBasicWeaknessContext -> m CardDef
sampleRandomBasicWeakness = sampleRandomBasicWeaknessExcluding []

{- | As 'sampleRandomBasicWeakness', but never draws a weakness whose canonical card code
is in @excluded@. Only one physical copy of each basic weakness exists, so a deck asking
for two random basic weaknesses must draw two different ones (#5424).

Exclusions are matched on 'canonicalCardCode' rather than 'CardDef' equality: two
printings of one weakness are different 'CardDef's (Mob Enforcer is 01101 in Core and
01601 in Revised Core), so comparing defs would let the same card through twice (#5264).

If the exclusions would empty the pool we fall back to the unrestricted one -- drawing a
repeat is far better than failing to build a deck at all.
-}
sampleRandomBasicWeaknessExcluding
  :: MonadRandom m => [CardCode] -> RandomBasicWeaknessContext -> m CardDef
sampleRandomBasicWeaknessExcluding excluded ctx =
  fromJustNote "No random basic weakness candidates"
    <$> sampleRandomBasicWeaknessMatching (const True) excluded ctx

{- | As 'sampleRandomBasicWeaknessExcluding', with an extra filter applied alongside the
built-in ones. Returns 'Nothing' when nothing at all matches -- an in-game search for a
weakness with a given trait has to be able to come back empty-handed.
-}
sampleRandomBasicWeaknessMatching
  :: MonadRandom m
  => (CardDef -> Bool) -> [CardCode] -> RandomBasicWeaknessContext -> m (Maybe CardDef)
sampleRandomBasicWeaknessMatching f excluded ctx =
  case nonEmpty (randomBasicWeaknessSamplingGroupsMatching f ctx) of
    Nothing -> pure Nothing
    Just groups -> do
      let allowed = filter (\(cardDef :| _) -> canonicalCardCode cardDef `notElem` excluded) (toList groups)
      printings <- sample $ fromMaybe groups (nonEmpty allowed)
      Just <$> sample printings

tabooMutate :: RandomBasicWeaknessContext -> CardDef -> CardDef
tabooMutate RandomBasicWeaknessContext {rbwTaboo} cardDef = maybe id tabooListModify rbwTaboo cardDef

randomBasicWeaknessCandidatesIgnoringCardPool
  :: (CardDef -> Bool) -> RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessCandidatesIgnoringCardPool f ctx =
  filter (\cardDef -> weaknessFilterIgnoringCardPool ctx cardDef && f cardDef)
    $ tabooMutate ctx
    <$> allBasicWeaknesses

weaknessFilter :: RandomBasicWeaknessContext -> CardDef -> Bool
weaknessFilter ctx cardDef =
  and
    [ multiplayerAllowed ctx cardDef
    , classAllowed ctx cardDef
    , standaloneAllowed ctx cardDef
    , cardPoolAllowed ctx cardDef
    ]

weaknessFilterIgnoringCardPool :: RandomBasicWeaknessContext -> CardDef -> Bool
weaknessFilterIgnoringCardPool ctx cardDef =
  and
    [ multiplayerAllowed ctx cardDef
    , classAllowed ctx cardDef
    , standaloneAllowed ctx cardDef
    ]

multiplayerAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
multiplayerAllowed RandomBasicWeaknessContext {rbwPlayerCount} cardDef =
  rbwPlayerCount >= 2 || MultiplayerOnly `notElem` cdDeckRestrictions cardDef

classAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
classAllowed RandomBasicWeaknessContext {rbwInvestigatorClass} cardDef =
  all restrictionAllowsClass $ cdDeckRestrictions cardDef
 where
  restrictionAllowsClass = \case
    OnlyClass c -> c == rbwInvestigatorClass
    _ -> True

standaloneAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
standaloneAllowed RandomBasicWeaknessContext {rbwStandalone} cardDef =
  not rbwStandalone || CampaignModeOnly `notElem` cdDeckRestrictions cardDef

cardPoolAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
cardPoolAllowed RandomBasicWeaknessContext {rbwCardPool} = cardPoolAllows rbwCardPool . toCardCode
