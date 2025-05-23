module Arkham.Helpers.Card (
  module Arkham.Helpers.Card,
  module Arkham.Helpers.Campaign,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ActiveCost.Base
import Arkham.Asset.Types
import Arkham.Card
import Arkham.ChaosBag.Base (chaosBagChaosTokens)
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.Entities
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.GameValue (gameValueMatches)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario (scenarioFieldMap)
import {-# SOURCE #-} Arkham.Helpers.Window (windowMatches)
import Arkham.Id
import Arkham.Investigator.Types qualified as Field
import Arkham.Keyword (Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types
import Arkham.Matcher hiding (AssetCard, LocationCard)
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Types
import Arkham.Window (Window)
import Data.List.Extra (nubOrdOn)
import Data.Proxy

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness

isWeakness :: IsCard card => card -> Bool
isWeakness c = case toCard c of
  PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
  EncounterCard _ -> True -- maybe?
  VengeanceCard _ -> False -- should be an error

getCardPayments :: HasGame m => Card -> m (Maybe Payment)
getCardPayments c = do
  costs <- getActiveCosts
  pure $ activeCostPayments <$> find (isCardTarget . activeCostTarget) costs
 where
  isCardTarget = \case
    ForAbility {} -> False
    ForAdditionalCost {} -> False
    ForCard _ c' -> toCardId c == toCardId c'
    ForCost c' -> toCardId c == toCardId c'

extendedCardMatch
  :: (HasGame m, IsCard c) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher =
  selectAny (basic (CardWithId c.id) <> matcher)

class ConvertToCard a where
  convertToCard :: (HasCallStack, HasGame m) => a -> m Card

instance ConvertToCard TreacheryId where
  convertToCard = getEntityCard @Treachery

instance ConvertToCard EnemyId where
  convertToCard eid =
    fmap (fromJustNote ("Could not convert entity: " <> show eid) . asum)
      $ sequence
      $ getEntityCardMaybe @Enemy eid
      : overOutOfPlayZones (\(_ :: Proxy zone) -> getEntityCardMaybe @(OutOfPlayEntity zone Enemy) eid)

instance ConvertToCard AssetId where
  convertToCard = getEntityCard @Asset

instance ConvertToCard LocationId where
  convertToCard = getEntityCard @Location

instance ConvertToCard Card where
  convertToCard = pure

instance ConvertToCard CardId where
  convertToCard = getCard

class (Projection a, Entity a) => CardEntity a where
  cardField :: Field a Card

getEntityCard :: forall a m. (HasCallStack, CardEntity a, HasGame m) => EntityId a -> m Card
getEntityCard = field (cardField @a)

getEntityCardMaybe :: forall a m. (CardEntity a, HasGame m) => EntityId a -> m (Maybe Card)
getEntityCardMaybe = fieldMay (cardField @a)

instance CardEntity Treachery where
  cardField = TreacheryCard

instance CardEntity Enemy where
  cardField = EnemyCard

instance KnownOutOfPlayZone zone => CardEntity (OutOfPlayEntity zone Enemy) where
  cardField = OutOfPlayEnemyField (knownOutOfPlayZone (Proxy @zone)) EnemyCard

instance CardEntity Asset where
  cardField = AssetCard

instance CardEntity Location where
  cardField = LocationCard

getCardField :: (HasCallStack, ConvertToCard c, HasGame m) => (CardDef -> a) -> c -> m a
getCardField f c = f . toCardDef <$> convertToCard c

getVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getVictoryPoints c = do
  card <- convertToCard c
  printedVictory <- getPrintedVictoryPoints card
  modifiers' <- getModifiers $ toCardId card
  if LoseVictory `elem` modifiers'
    then pure Nothing
    else pure $ foldr applyModifier printedVictory modifiers'
 where
  applyModifier (GainVictory n) m = Just (n + fromMaybe 0 m)
  applyModifier _ n = n

getHasVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m Bool
getHasVictoryPoints c = isJust <$> getVictoryPoints c

getPrintedVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getPrintedVictoryPoints = getCardField cdVictoryPoints

-- To get abilities we convert to some entity in Entities and get all abilities
getCardAbilities :: InvestigatorId -> Card -> [Ability]
getCardAbilities iid c = getAbilities $ addCardEntityWith iid id mempty c

findJustCard :: HasGame m => (Card -> Bool) -> m Card
findJustCard cardPred = fromJustNote "invalid card" <$> findCard cardPred

findUniqueCard :: HasGame m => CardDef -> m Card
findUniqueCard def = findJustCard (`cardMatch` (cardIs def <> CardIsUnique))

iconsForCard :: HasGame m => Card -> m [SkillIcon]
iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
  mods <- getModifiers (CardIdTarget pcId)
  let wildReplace = if ReplaceAllSkillIconsWithWild `elem` mods then const #wild else id
  pure
    $ map wildReplace
    $ foldr applyAfter (foldr apply (cdSkills $ toCardDef c) mods) mods
 where
  apply (AddSkillIcons xs) ys = xs <> ys
  apply (RemoveSkillIcons xs) ys = ys \\ xs
  apply _ ys = ys
  applyAfter DoubleSkillIcons ys = ys <> ys
  applyAfter _ ys = ys
iconsForCard _ = pure []

getCardEntityTarget :: HasGame m => Card -> m (Maybe Target)
getCardEntityTarget card = case toCardType card of
  EnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  PlayerEnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  TreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  PlayerTreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  LocationType -> toTarget <$$> selectOne (LocationWithCardId $ toCardId card)
  AssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  EncounterAssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  SkillType -> toTarget <$$> selectOne (SkillWithCardId $ toCardId card)
  other -> error $ "Unhandled type: " <> show other

-- NOTE: this used to call drawThisPlayerCard but we changed that because we
-- weren't triggering Foresight (1). If we have to revert this, remember to
-- check foresight (1).
drawThisCardFrom :: IsCard card => InvestigatorId -> card -> Maybe DeckSignifier -> [Message]
drawThisCardFrom iid card mdeck = case toCard card of
  c@(PlayerCard pc) -> [InvestigatorDrewPlayerCardFrom iid pc mdeck, ResolvedCard iid c]
  EncounterCard _ -> error "Not yet implemented"
  VengeanceCard c -> drawThisCardFrom iid c mdeck

-- drawThisPlayerCard :: InvestigatorId -> PlayerCard -> [Message]
-- drawThisPlayerCard iid card = case toCardType card of
--   PlayerTreacheryType ->
--     [ DrewTreachery iid (Just $ Deck.InvestigatorDeck iid) (PlayerCard card)
--     , ResolvedCard iid (PlayerCard card)
--     ]
--   PlayerEnemyType -> do
--     if hasRevelation card
--       then [Revelation iid $ CardIdSource card.id, ResolvedCard iid (PlayerCard card)]
--       else [DrewPlayerEnemy iid (PlayerCard card), ResolvedCard iid (PlayerCard card)]
--   other | hasRevelation card && other `notElem` [PlayerTreacheryType, PlayerEnemyType] -> do
--     [Revelation iid (CardIdSource card.id), ResolvedCard iid (PlayerCard card)]
--   _ -> []

playIsValidAfterSeal :: HasGame m => InvestigatorId -> Card -> m Bool
playIsValidAfterSeal iid c = do
  tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
  let
    sealingToMatcher = \case
      Sealing matcher -> Just matcher
      SealOneOf (m1 :| rest) -> Just $ oneOf $ mapMaybe sealingToMatcher (m1 : rest)
      SealUpTo _ matcher -> Just matcher
      SealUpToX _ -> Nothing
    sealChaosTokenMatchers = flip mapMaybe (setToList c.keywords) \case
      Keyword.Seal sealing -> sealingToMatcher sealing
      _ -> Nothing
  allM (\matcher -> anyM (\t -> matchChaosToken iid t matcher) tokens) sealChaosTokenMatchers

cardListMatches :: HasGame m => [Card] -> Matcher.CardListMatcher -> m Bool
cardListMatches cards = \case
  Matcher.AnyCards -> pure $ notNull cards
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.DifferentLengthIsAtLeast n cardMatcher -> pure $ length (nubOrdOn toTitle $ filter (`cardMatch` cardMatcher) cards) >= n
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards
  Matcher.NoCards -> pure $ null cards

passesLimits :: HasGame m => InvestigatorId -> Card -> m Bool
passesLimits iid c = allM go (cdLimits $ toCardDef c)
 where
  go = \case
    LimitInPlay m -> case toCardType c of
      EventType -> do
        n <- selectCount $ Matcher.EventWithTitle (nameTitle $ toName c)
        pure $ m > n
      AssetType -> do
        n <- selectCount $ Matcher.AssetWithTitle (nameTitle $ toName c)
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    LimitPerInvestigator m -> case toCardType c of
      AssetType -> do
        n <- selectCount $ Matcher.assetControlledBy iid <> Matcher.AssetWithTitle (nameTitle $ toName c)
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    LimitPerTrait t m -> case toCardType c of
      AssetType -> do
        n <- selectCount (Matcher.assetControlledBy iid <> Matcher.AssetWithTrait t)
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerAttack m -> case toCardType c of
      EventType -> do
        n <- selectCount $ Matcher.eventIs c
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerGame m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTurn m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerRound m -> do
      n <- getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTraitPerRound t m -> do
      n <- count (elem t) . map toTraits <$> getAllCardUses
      pure $ m > n

cardInFastWindows
  :: HasGame m
  => InvestigatorId
  -> Source
  -> Card
  -> [Window]
  -> Matcher.WindowMatcher
  -> m Bool
cardInFastWindows iid source card windows' matcher =
  anyM (\window -> windowMatches iid source' window matcher) windows'
 where
  source' = case card of
    PlayerCard pc -> BothSource source (CardIdSource pc.id)
    _ -> source

getPotentiallyModifiedCardCost
  :: HasGame m => InvestigatorId -> Card -> Bool -> Int -> m Int
getPotentiallyModifiedCardCost iid c@(PlayerCard _) excludeChuckFergus startingCost = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (ChuckFergus2Modifier cardMatcher m) | not excludeChuckFergus = do
    -- get is playable will check if this has to be used, will likely break if
    -- anything else aside from Chuck Fergus (2) interacts with this
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost iid c@(EncounterCard _) excludeChuckFergus _ = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (CanReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (ChuckFergus2Modifier cardMatcher m) | not excludeChuckFergus = do
    -- get is playable will check if this has to be used
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n _ = pure n
getPotentiallyModifiedCardCost _ (VengeanceCard _) _ _ =
  error "should not check vengeance card"

getModifiedCardCost :: HasGame m => InvestigatorId -> Card -> m Int
getModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers iid
  cardModifiers <- getModifiers c.id
  startingCost <- getStartingCost
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  pcDef = toCardDef c
  getStartingCost = case cdCost pcDef of
    Just (StaticCost n) -> pure n
    Just DynamicCost -> pure 0
    Just (MaxDynamicCost _) -> pure 0
    Just DiscardAmountCost -> fieldMap Field.InvestigatorDiscard (count ((== toCardCode c) . toCardCode)) iid
    Nothing -> pure 0
  -- A card like The Painted World which has no cost, but can be "played", should not have it's cost modified
  applyModifier n _ | isNothing (cdCost pcDef) = pure n
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    ok <- case cardMatcher of
      BasicCardMatch inner -> pure $ c `cardMatch` inner
      _ -> c <=~> cardMatcher
    pure $ if ok then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    ok <- case cardMatcher of
      BasicCardMatch inner -> pure $ c `cardMatch` inner
      _ -> c <=~> cardMatcher
    pure $ if ok then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost _ (VengeanceCard _) =
  error "should not happen for vengeance"
