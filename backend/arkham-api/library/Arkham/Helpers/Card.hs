module Arkham.Helpers.Card (
  module Arkham.Helpers.Card,
  module Arkham.Helpers.Campaign,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types
import Arkham.Card
import Arkham.ChaosBag.Base (chaosBagChaosTokens)
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Enemy.Types
import Arkham.Event.Types qualified as Field
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
import Arkham.Tracing
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
extendedCardMatch
  :: (HasGame m, Tracing m, IsCard c) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher =
  selectAny (basic (CardWithId c.id) <> matcher)

class ConvertToCard a where
  convertToCard :: (HasCallStack, HasGame m, Tracing m) => a -> m Card

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

getEntityCard
  :: forall a m. (HasCallStack, CardEntity a, HasGame m, Tracing m) => EntityId a -> m Card
getEntityCard = field (cardField @a)

getEntityCardMaybe
  :: forall a m. (CardEntity a, HasGame m, Tracing m) => EntityId a -> m (Maybe Card)
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

getCardField :: (HasCallStack, ConvertToCard c, HasGame m, Tracing m) => (CardDef -> a) -> c -> m a
getCardField f c = f . toCardDef <$> convertToCard c

getVictoryPoints :: (ConvertToCard c, HasGame m, Tracing m) => c -> m (Maybe Int)
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

getHasVictoryPoints :: (ConvertToCard c, HasGame m, Tracing m) => c -> m Bool
getHasVictoryPoints c = isJust <$> getVictoryPoints c

getPrintedVictoryPoints :: (ConvertToCard c, HasGame m, Tracing m) => c -> m (Maybe Int)
getPrintedVictoryPoints = getCardField cdVictoryPoints

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

getCardEntityTarget :: (HasGame m, Tracing m) => Card -> m (Maybe Target)
getCardEntityTarget card = case toCardType card of
  EnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  PlayerEnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  TreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  PlayerTreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  LocationType -> toTarget <$$> selectOne (LocationWithCardId $ toCardId card)
  AssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  EncounterAssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  EventType -> toTarget <$$> selectOne (EventWithCardId $ toCardId card)
  EncounterEventType -> toTarget <$$> selectOne (EventWithCardId $ toCardId card)
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

playIsValidAfterSeal :: (HasGame m, Tracing m) => InvestigatorId -> Card -> m Bool
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

cardListMatches :: (HasGame m, Tracing m) => [Card] -> Matcher.CardListMatcher -> m Bool
cardListMatches cards = \case
  Matcher.AnyCards -> pure $ notNull cards
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.DifferentLengthIsAtLeast n cardMatcher -> pure $ length (nubOrdOn toTitle $ filter (`cardMatch` cardMatcher) cards) >= n
  Matcher.NotCards cardMatcher -> not <$> cardListMatches cards cardMatcher
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards
  Matcher.NoCards -> pure $ null cards

passesLimits :: (HasGame m, Tracing m) => InvestigatorId -> Card -> m Bool
passesLimits iid c = do
  perLimitOk <- allM go (cdLimits $ toCardDef c)
  -- 'LimitPerTraitPerLocation' is resolved against the candidate play
  -- location(s) below. It is checked for every card, not just cards that carry
  -- the limit, so that the mirror rule holds: a card may not be played onto a
  -- location that already holds a card limiting one of its traits (e.g. an
  -- unlimited trap cannot be played onto a limited trap, and vice versa).
  perLocationOk <- passesPerLocationLimits iid c
  pure $ perLimitOk && perLocationOk
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
        mods <- getModifiers iid
        let othersMatchers = [imatch | CanModify (CanPlayUnderControlOf cmatch imatch) <- mods, cardMatch c cmatch]
        if null othersMatchers
          then do
            n <- selectCount $ Matcher.assetControlledBy iid <> Matcher.AssetWithTitle (nameTitle $ toName c)
            pure $ m > n
          else do
            iids <- select (investigator_ $ oneOf othersMatchers)
            (iid : iids) & nub & anyM \iid' -> do
              n <- selectCount $ Matcher.assetControlledBy iid' <> Matcher.AssetWithTitle (nameTitle $ toName c)
              pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    LimitPerTrait t m -> case toCardType c of
      AssetType -> do
        mods <- getModifiers iid
        let othersMatchers = [imatch | CanModify (CanPlayUnderControlOf cmatch imatch) <- mods, cardMatch c cmatch]
        if null othersMatchers
          then do
            n <- selectCount (Matcher.assetControlledBy iid <> Matcher.AssetWithTrait t)
            pure $ m > n
          else do
            iids <- select (investigator_ $ oneOf othersMatchers)
            (iid : iids) & nub & anyM \iid' -> do
              n <- selectCount (Matcher.assetControlledBy iid' <> Matcher.AssetWithTrait t)
              pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerAttack m -> case toCardType c of
      EventType -> do
        n <- selectCount $ Matcher.eventIs c
        pure $ m > n
      _ -> error $ "Not handling card type: " <> show (toCardType c)
    MaxPerGame m -> do
      n <- length <$> getCardUses (toCardCode c)
      pure $ m > n
    MaxPerGamePerInvestigator m -> do
      n <- count (== iid) <$> getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTurn m -> do
      n <- length <$> getCardUses (toCardCode c)
      pure $ m > n
    MaxPerRound m -> do
      n <- length <$> getCardUses (toCardCode c)
      pure $ m > n
    LimitPerRound m -> do
      n <- count (== iid) <$> getCardUses (toCardCode c)
      pure $ m > n
    MaxPerTraitPerRound t m -> do
      n <- count (elem t) . map toTraits <$> getAllCardUses
      pure $ m > n
    -- Handled in 'passesPerLocationLimits' so it applies at the play location
    -- and so the mirror exclusion is enforced for unlimited cards too.
    LimitPerTraitPerLocation {} -> pure True

-- | The traits a card limits to "N per location" via 'LimitPerTraitPerLocation'.
perLocationLimitTraits :: Card -> [(Trait, Int)]
perLocationLimitTraits c = [(t, m) | LimitPerTraitPerLocation t m <- cdLimits (toCardDef c)]

-- | Every card currently attached to a location (events, assets, treacheries).
-- Used by the per-trait-per-location limit checks.
attachedCardsAt :: (HasGame m, Tracing m) => LocationId -> m [Card]
attachedCardsAt lid = do
  let target = TargetIs (toTarget lid)
  eventCards <- traverse (field Field.EventCard) =<< select (EventAttachedTo target)
  assetCards <- traverse (field AssetCard) =<< select (AssetAttachedTo target)
  treacheryCards <-
    traverse (field TreacheryCard) =<< select (TreacheryAttachedToLocation $ LocationWithId lid)
  pure $ eventCards <> assetCards <> treacheryCards

-- | Resolve 'LimitPerTraitPerLocation' at the location(s) where @c@ would be
-- played: the investigator's location plus any locations granted by a
-- @CanPlayAtLocation@ modifier. The card is allowed if it satisfies the limit
-- at any one candidate location.
passesPerLocationLimits :: (HasGame m, Tracing m) => InvestigatorId -> Card -> m Bool
passesPerLocationLimits iid c = do
  baseLids <- select (locationWithInvestigator iid)
  -- Vacuously true when the investigator is at no location.
  baseOk <- allM (cardPassesLimitsAtLocation c) baseLids
  if baseOk
    then pure True
    else do
      -- Only consult modifiers when the base location fails: a 'CanPlayAtLocation'
      -- modifier may allow the card to be played at another location instead.
      mods <- getModifiers iid
      let
        extraMatchers =
          [ lmatch
          | CanModify (CanPlayAtLocation cmatch lmatch) <- mods
          , cardMatch c cmatch
          ]
      if null extraMatchers
        then pure False
        else do
          extraLids <- nub . concat <$> traverse select extraMatchers
          anyM (cardPassesLimitsAtLocation c) extraLids

-- | Whether @c@ may be placed at @lid@ under the "N <trait> per location"
-- rules. This enforces both directions:
--
--   * @c@'s own 'LimitPerTraitPerLocation' limits, counting every attached card
--     (event, asset, or treachery) that shares the limited trait; and
--   * the mirror rule, that @c@ may not join a location already holding a card
--     that limits one of @c@'s traits to "N per location".
--
-- Together these guarantee a limited trap is always alone: it cannot be placed
-- where any trap exists, and no trap can be placed where it sits.
cardPassesLimitsAtLocation :: (HasGame m, Tracing m) => Card -> LocationId -> m Bool
cardPassesLimitsAtLocation c lid = do
  attached <- attachedCardsAt lid
  let
    cTraits = cdCardTraits (toCardDef c)
    sharesTrait t x = t `elem` cdCardTraits (toCardDef x)
    ownLimitsOk =
      all (\(t, m) -> m > length (filter (sharesTrait t) attached)) (perLocationLimitTraits c)
    notExcluded =
      not $ any (\x -> any ((`elem` cTraits) . fst) (perLocationLimitTraits x)) attached
  pure $ ownLimitsOk && notExcluded

cardIsFast :: HasGame m => Card -> m Bool
cardIsFast = cardIsFast' getModifiers

cardIsFast' :: HasGame m => (Card -> m [ModifierType]) -> Card -> m Bool
cardIsFast' fetchModifiers card = do
  if isJust $ cdFastWindow (toCardDef card)
    then pure True
    else do
      allModifiers <- fetchModifiers card
      pure $ isJust $ listToMaybe [w | BecomesFast w <- allModifiers]

cardInFastWindows
  :: (Tracing m, HasGame m, HasCallStack)
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
  modifiers <- getModifiers iid
  cardModifiers <- getModifiers $ toCardId c
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
  modifiers <- getModifiers iid
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

getModifiedCardCost :: (HasGame m, Tracing m) => InvestigatorId -> Card -> m (Maybe Int)
getModifiedCardCost iid c = fmap (max 0) <$> getUnboundedModifiedCardCost iid c

getUnboundedModifiedCardCost :: (HasGame m, Tracing m) => InvestigatorId -> Card -> m (Maybe Int)
getUnboundedModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers iid
  cardModifiers <- getModifiers c.id
  mStartingCost <- getStartingCost
  for mStartingCost \startingCost ->
    foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  pcDef = toCardDef c
  getStartingCost = case cdCost pcDef of
    Just (StaticCost n) -> pure $ Just n
    Just DynamicCost -> pure $ Just 0
    Just (MaxDynamicCost _) -> pure $ Just 0
    Just DeferredCost -> pure $ Just 0
    Just (MatchingEnemyFieldCost matcher fld) -> do
      let
        fld' =
          case fld of
            EnemyRemainingHealthField -> EnemyRemainingHealth
      vals <- catMaybes <$> selectField fld' matcher
      pure $ case sort vals of
        [] -> Nothing
        (x : _) -> Just x
    Just (AnyMatchingCardCost ecMatcher) -> do
      cards <- select ecMatcher
      pure $ case minsBy getCost cards of
        [] -> Nothing
        (x : _) -> Just $ getCost x
    Just DiscardAmountCost -> fieldMap Field.InvestigatorDiscard (Just . count ((== toCardCode c) . toCardCode)) iid
    Nothing -> pure Nothing
  -- A card like The Painted World which has a deferred cost, but can be "played", should not have it's cost modified
  applyModifier n _ | cdCost pcDef == Just DeferredCost = pure n
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then n - m else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    ok <- case cardMatcher of
      BasicCardMatch inner -> pure $ c `cardMatch` inner
      _ -> c <=~> cardMatcher
    pure $ if ok then n + m else n
  applyModifier n _ = pure n
getUnboundedModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorTarget iid)
  Just
    <$> foldM
      applyModifier
      (error "we need so specify ecCost for this to work")
      modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then n - m else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    ok <- case cardMatcher of
      BasicCardMatch inner -> pure $ c `cardMatch` inner
      _ -> c <=~> cardMatcher
    pure $ if ok then n + m else n
  applyModifier n _ = pure n
getUnboundedModifiedCardCost _ (VengeanceCard _) =
  error "should not happen for vengeance"
