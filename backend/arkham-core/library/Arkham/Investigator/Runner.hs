{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Runner (
  module Arkham.Investigator.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability as X hiding (PaidCost)
import Arkham.ChaosToken as X
import Arkham.ClassSymbol as X
import Arkham.Classes as X
import Arkham.Helpers.Investigator as X
import Arkham.Helpers.Message as X
import Arkham.Investigator.Types as X
import Arkham.Message as X hiding (InvestigatorDamage, InvestigatorDefeated, InvestigatorResigned)
import Arkham.Name as X
import Arkham.Source as X
import Arkham.Stats as X
import Arkham.Target as X
import Arkham.Trait as X hiding (Cultist)

import Arkham.Action (Action)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.CommitRestriction
import Arkham.Cost qualified as Cost
import Arkham.Damage
import Arkham.DamageEffect
import Arkham.Deck qualified as Deck
import Arkham.DefeatedBy
import Arkham.Discard
import Arkham.Draw.Types
import Arkham.Enemy.Types qualified as Field
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.Game (withoutCanModifiers)
import Arkham.Game.Helpers hiding (windows)
import Arkham.Game.Helpers qualified as Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Card (extendedCardMatch, iconsForCard)
import Arkham.Helpers.Deck qualified as Deck
import Arkham.Id
import Arkham.Investigator.Types qualified as Attrs
import Arkham.Key
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  AssetMatcher (..),
  CardMatcher (..),
  EnemyMatcher (..),
  EventMatcher (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  assetControlledBy,
  assetIs,
  colocatedWith,
  treacheryInHandOf,
  pattern InvestigatorCanDisengage,
 )
import Arkham.Message qualified as Msg
import Arkham.Modifier qualified as Modifier
import Arkham.Movement
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.SkillTest
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window
import Arkham.Zone qualified as Zone
import Control.Lens (each, non)
import Control.Monad.Extra (findM)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set qualified as Set

instance RunMessage InvestigatorAttrs where
  runMessage = runInvestigatorMessage

onlyCampaignAbilities :: UsedAbility -> Bool
onlyCampaignAbilities UsedAbility {..} = case abilityLimitType (abilityLimit usedAbility) of
  Just PerCampaign -> True
  _ -> False

-- There are a few conditions that can occur that mean we must need to use an ability.
-- No valid targets. For example Marksmanship
-- Can't afford card. For example On Your Own
getAllAbilitiesSkippable :: InvestigatorAttrs -> [Window] -> GameT Bool
getAllAbilitiesSkippable attrs windows =
  allM (getWindowSkippable attrs windows) windows

getWindowSkippable :: InvestigatorAttrs -> [Window] -> Window -> GameT Bool
getWindowSkippable attrs ws (windowType -> Window.PlayCard iid card@(PlayerCard pc)) | iid == toId attrs = do
  modifiers' <- getModifiers (toCardId card)
  modifiers'' <- getModifiers (CardTarget card)
  cost <- getModifiedCardCost iid card
  let allModifiers = modifiers' <> modifiers''
  let isFast = isJust (cdFastWindow $ toCardDef pc) || BecomesFast `elem` allModifiers
  andM
    [ getCanAffordCost (toId attrs) pc (Just #play) ws (ResourceCost cost)
    , if isFast
        then pure True
        else getCanAffordCost (toId attrs) pc (Just #play) ws (ActionCost 1)
    ]
getWindowSkippable _ _ w@(windowType -> Window.ActivateAbility iid ab) = do
  let
    excludeOne [] = []
    excludeOne (uab : xs) | ab == usedAbility uab = do
      if usedTimes uab <= 1
        then xs
        else uab {usedTimes = usedTimes uab - 1} : xs
    excludeOne (uab : xs) = uab : excludeOne xs
  andM
    [ getCanAffordUseWith excludeOne CanNotIgnoreAbilityLimit iid ab w
    , withAlteredGame withoutCanModifiers
        $ passesCriteria iid Nothing (abilitySource ab) [w] (abilityCriteria ab)
    ]
getWindowSkippable _ _ _ = pure True

getHealthDamageableAssets
  :: HasGame m
  => InvestigatorId
  -> AssetMatcher
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getHealthDamageableAssets _ _ 0 _ _ = pure mempty
getHealthDamageableAssets iid matcher _ damageTargets horrorTargets = do
  allAssets <- selectList $ matcher <> AssetCanBeAssignedDamageBy iid
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectListMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  pure $ setFromList $ filter (`notMember` excludes) allAssets

getSanityDamageableAssets
  :: HasGame m
  => InvestigatorId
  -> AssetMatcher
  -> Int
  -> [Target]
  -> [Target]
  -> m (Set AssetId)
getSanityDamageableAssets _ _ 0 _ _ = pure mempty
getSanityDamageableAssets iid matcher _ damageTargets horrorTargets = do
  allAssets <- selectList $ matcher <> AssetCanBeAssignedHorrorBy iid
  excludes <- do
    modifiers <- getModifiers iid
    excludeMatchers <- forMaybeM modifiers $ \case
      NoMoreThanOneDamageOrHorrorAmongst excludeMatcher -> do
        excludes <- selectListMap AssetTarget excludeMatcher
        pure $ do
          guard $ any (`elem` excludes) (damageTargets <> horrorTargets)
          pure excludeMatcher
      _ -> pure Nothing
    case excludeMatchers of
      [] -> pure mempty
      xs -> select (AssetOneOf xs)
  pure $ setFromList $ filter (`notMember` excludes) allAssets

runInvestigatorMessage
  :: Message -> InvestigatorAttrs -> GameT InvestigatorAttrs
runInvestigatorMessage msg a@InvestigatorAttrs {..} = case msg of
  EndCheckWindow -> do
    depth <- getWindowDepth
    let
      filterAbility UsedAbility {..} = do
        limit <- getAbilityLimit (toId a) usedAbility
        pure $ case limit of
          NoLimit -> False
          PlayerLimit PerWindow _ -> depth > usedDepth
          GroupLimit PerWindow _ -> depth > usedDepth
          _ -> True

    usedAbilities' <- filterM filterAbility investigatorUsedAbilities

    pure $ a & usedAbilitiesL .~ usedAbilities'
  ResetGame ->
    pure
      $ (cbCardBuilder (investigator id (toCardDef a) (getAttrStats a)) nullCardId ())
        { Attrs.investigatorId = investigatorId
        , investigatorXp = investigatorXp
        , investigatorPhysicalTrauma = investigatorPhysicalTrauma
        , investigatorMentalTrauma = investigatorMentalTrauma
        , investigatorTokens =
            addTokens Horror investigatorMentalTrauma $ addTokens Token.Damage investigatorPhysicalTrauma mempty
        , investigatorStartsWith = investigatorStartsWith
        , investigatorStartsWithInHand = investigatorStartsWithInHand
        , investigatorSupplies = investigatorSupplies
        , investigatorUsedAbilities = filter onlyCampaignAbilities investigatorUsedAbilities
        }
  SetupInvestigator iid | iid == investigatorId -> do
    (startsWithMsgs, deck') <-
      foldM
        ( \(msgs, currentDeck) cardDef -> do
            let (before, after) = break ((== cardDef) . toCardDef) (unDeck currentDeck)
            case after of
              (card : rest) ->
                pure
                  ( PutCardIntoPlay
                      investigatorId
                      (toCard card)
                      Nothing
                      (Window.defaultWindows investigatorId)
                      : msgs
                  , Deck (before <> rest)
                  )
              _ -> do
                card <- genCard cardDef
                let
                  setOwner = \case
                    PlayerCard pc -> PlayerCard $ pc {pcOwner = Just investigatorId}
                    other -> other
                pure
                  ( PutCardIntoPlay
                      investigatorId
                      (setOwner card)
                      Nothing
                      (Window.defaultWindows investigatorId)
                      : msgs
                  , currentDeck
                  )
        )
        ([], investigatorDeck)
        investigatorStartsWith
    let (permanentCards, deck'') = partition (cdPermanent . toCardDef) (unDeck deck')
    beforeDrawingStartingHand <- checkWindows [mkWhen (Window.DrawingStartingHand investigatorId)]
    let deck''' = filter ((`notElem` investigatorStartsWithInHand) . toCardDef) deck''
    pushAll
      $ startsWithMsgs
      <> [ PutCardIntoPlay investigatorId (PlayerCard card) Nothing (Window.defaultWindows investigatorId)
         | card <- permanentCards
         ]
      <> [ beforeDrawingStartingHand
         , DrawStartingHand investigatorId
         , TakeStartingResources investigatorId
         ]
    pure $ a & (deckL .~ Deck deck''')
  DrawStartingHand iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    if any (`elem` modifiers') [CannotDrawCards, CannotManipulateDeck]
      then pure a
      else do
        let
          startingHandAmount = foldr applyModifier 5 modifiers'
          applyModifier (StartingHand m) n = max 0 (n + m)
          applyModifier _ n = n
        (discard, hand, deck) <- drawOpeningHand a startingHandAmount
        pure
          $ a
          & (discardL .~ discard)
          & (handL .~ hand)
          & (deckL .~ Deck deck)
  ReturnToHand iid (AssetTarget aid) | iid == investigatorId -> do
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  ReturnToHand iid (CardTarget card) | iid == investigatorId -> do
    -- Card is assumed to be in your discard
    -- but since find card can also return cards in your hand
    -- we filter again just in case
    let
      discardFilter = case preview _PlayerCard card of
        Just pc -> filter (/= pc)
        Nothing -> id
    pure
      $ a
      & (discardL %~ discardFilter)
      & (handL %~ filter (/= card))
      & (handL %~ (card :))
  CheckAdditionalActionCosts iid _ action msgs | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    let
      additionalCosts =
        mapMaybe
          ( \case
              ActionCostOf (IsAction action') n | action == action' -> Just (ActionCost n)
              _ -> Nothing
          )
          modifiers'
    if null additionalCosts
      then pushAll msgs
      else do
        canPay <-
          getCanAffordCost iid a Nothing [mkWhen Window.NonFast] (mconcat additionalCosts)
        when canPay
          $ pushAll
          $ [PayForAbility (abilityEffect a $ mconcat additionalCosts) []]
          <> msgs
    pure a
  TakeStartingResources iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    let
      startingResources =
        foldl'
          ( \total -> \case
              StartingResources n -> max 0 (total + n)
              _ -> total
          )
          5
          modifiers'
      startingClues =
        foldl'
          ( \total -> \case
              StartingClues n -> max 0 (total + n)
              _ -> total
          )
          0
          modifiers'
    pure $ a & tokensL %~ (setTokens Resource startingResources . setTokens Clue startingClues)
  InvestigatorMulligan iid | iid == investigatorId -> do
    unableToMulligan <- hasModifier a CannotMulligan
    push
      $ if null investigatorHand || unableToMulligan
        then FinishedWithMulligan investigatorId
        else
          chooseOne iid
            $ Label "Done With Mulligan" [FinishedWithMulligan investigatorId]
            : [ targetLabel (toCardId card)
                $ [DiscardCard iid GameSource (toCardId card), InvestigatorMulligan iid]
              | card <- investigatorHand
              , cdCanReplace (toCardDef card)
              ]
    pure a
  BeginTrade iid _source (AssetTarget aid) iids | iid == investigatorId -> do
    push $ chooseOne iid [targetLabel iid' [TakeControlOfAsset iid' aid] | iid' <- iids]
    pure a
  BeginTrade iid source ResourceTarget iids | iid == investigatorId -> do
    push
      $ chooseOne iid
      $ [targetLabel iid' [TakeResources iid' 1 source False, SpendResources iid 1] | iid' <- iids]
    pure a
  SetRole iid role | iid == investigatorId -> do
    pure $ a {investigatorClass = role}
  AllRandomDiscard source matcher | not (a ^. defeatedL || a ^. resignedL) -> do
    push $ toMessage $ randomDiscardMatching investigatorId source matcher
    pure a
  FinishedWithMulligan iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    let
      allowedMulligans =
        foldl'
          ( \total -> \case
              Mulligans n -> max 0 (total + n)
              _ -> total
          )
          1
          modifiers'
      startingHandAmount =
        foldl'
          ( \total -> \case
              StartingHand n -> max 0 (total + n)
              _ -> total
          )
          5
          modifiers'
      startingResources =
        foldl'
          ( \total -> \case
              StartingResources n -> max 0 (total + n)
              _ -> total
          )
          5
          modifiers'
    (discard, hand, deck) <-
      if any (`elem` (modifiers')) [CannotDrawCards, CannotManipulateDeck]
        then pure (investigatorDiscard, investigatorHand, unDeck investigatorDeck)
        else drawOpeningHand a (startingHandAmount - length investigatorHand)
    window <- checkWindows [mkAfter (Window.DrawingStartingHand iid)]
    additionalHandCards <- traverse genCard investigatorStartsWithInHand

    -- if we have any cards with revelations on them, we need to trigger them
    let revelationCards = filter (hasRevelation . toCardDef) (additionalHandCards <> hand)
    let
      choices = mapMaybe cardChoice revelationCards
      cardChoice = \case
        card@(PlayerCard card') -> do
          if hasRevelation card'
            then
              if toCardType card' == PlayerTreacheryType
                then Just (card, DrewTreachery iid Nothing card)
                else Just (card, Revelation iid $ PlayerCardSource card')
            else
              if toCardType card' == PlayerEnemyType
                then Just (card, DrewPlayerEnemy iid card)
                else Nothing
        _ -> Nothing

    pushAll
      $ [ShuffleDiscardBackIn iid, window]
      <> [ chooseOrRunOneAtATime iid [targetLabel (toCardId card) [msg'] | (card, msg') <- choices]
         | notNull choices
         ]
      <> [ InvestigatorMulligan iid
         | (a ^. mulligansTakenL + 1) < allowedMulligans
         , startingHandAmount - length investigatorHand > 0
         ]
    pure
      $ a
      & (tokensL %~ setTokens Resource startingResources)
      & (discardL .~ discard)
      & (handL .~ hand <> additionalHandCards)
      & (deckL .~ Deck deck)
      & (mulligansTakenL +~ 1)
  ShuffleDiscardBackIn iid | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    if null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` modifiers'
      then pure a
      else do
        deck <- shuffleM (investigatorDiscard <> coerce investigatorDeck)
        pure $ a & discardL .~ [] & deckL .~ Deck deck
  Resign iid | iid == investigatorId -> do
    isLead <- (== iid) <$> getLeadInvestigatorId
    pushAll $ [ChooseLeadInvestigator | isLead] <> resolve (Msg.InvestigatorResigned iid)
    pure $ a & resignedL .~ True & endedTurnL .~ True
  Msg.InvestigatorDefeated source iid -> do
    -- a card effect defeats an investigator directly
    windowMsg <-
      checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated (DefeatedByOther source) (toId a)]
    pushAll [windowMsg, InvestigatorIsDefeated source iid]
    pure a
  InvestigatorIsDefeated source iid | iid == investigatorId -> do
    isLead <- (== iid) <$> getLeadInvestigatorId
    modifiedHealth <- getModifiedHealth a
    modifiedSanity <- getModifiedSanity a
    let
      defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
      defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
      defeatedBy = case (defeatedByHorror, defeatedByDamage) of
        (True, True) -> DefeatedByDamageAndHorror source
        (True, False) -> DefeatedByHorror source
        (False, True) -> DefeatedByDamage source
        (False, False) -> DefeatedByOther source
      physicalTrauma =
        if investigatorHealthDamage a >= modifiedHealth then 1 else 0
      mentalTrauma =
        if investigatorSanityDamage a >= modifiedSanity then 1 else 0
    windowMsg <- checkWindows [mkAfter $ Window.InvestigatorDefeated defeatedBy iid]
    killed <- hasModifier a KilledIfDefeated
    pushAll
      $ windowMsg
      : [ChooseLeadInvestigator | isLead]
        <> [InvestigatorKilled (toSource a) iid | killed]
        <> [InvestigatorWhenEliminated (toSource a) iid]
    pure
      $ a
      & (defeatedL .~ True)
      & (endedTurnL .~ True)
      & (physicalTraumaL +~ physicalTrauma)
      & (mentalTraumaL +~ mentalTrauma)
  Msg.InvestigatorResigned iid | iid == investigatorId -> do
    isLead <- (== iid) <$> getLeadInvestigatorId
    pushAll
      $ [ChooseLeadInvestigator | isLead && not investigatorResigned]
      <> [InvestigatorWhenEliminated (toSource a) iid]
    pure $ a & resignedL .~ True & endedTurnL .~ True
  -- InvestigatorWhenEliminated is handled by the scenario
  InvestigatorEliminated iid | iid == investigatorId -> do
    pushAll
      $ PlaceTokens (toSource a) (toTarget investigatorLocation) Clue (investigatorClues a)
      : [PlaceKey (toTarget investigatorLocation) k | k <- toList investigatorKeys]
    pure $ a & tokensL %~ (removeAllTokens Clue . removeAllTokens Resource) & keysL .~ mempty
  EnemyMove eid lid | lid /= investigatorLocation -> do
    pure $ a & engagedEnemiesL %~ deleteSet eid
  EnemyEngageInvestigator eid iid | iid == investigatorId -> do
    pure $ a & engagedEnemiesL %~ insertSet eid
  EnemyEngageInvestigator eid iid | iid /= investigatorId -> do
    pure $ a & engagedEnemiesL %~ deleteSet eid
  RemoveAllClues _ (InvestigatorTarget iid) | iid == investigatorId -> do
    pure $ a & tokensL %~ removeAllTokens Clue
  RemoveEnemy eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  RemovedFromPlay source@(AssetSource _) ->
    pure $ a & (slotsL . each %~ filter ((/= source) . slotSource))
  RemovedFromPlay (EnemySource eid) ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  TakeControlOfAsset iid aid | iid == investigatorId -> do
    a <$ push (InvestigatorPlayAsset iid aid)
  TakeControlOfAsset iid aid | iid /= investigatorId -> do
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  ChooseAndDiscardAsset iid source assetMatcher | iid == investigatorId -> do
    discardableAssetIds <- selectList $ assetMatcher <> DiscardableAsset <> assetControlledBy iid
    push
      $ chooseOrRunOne iid
      $ map (\aid -> targetLabel aid [Discard source $ AssetTarget aid]) discardableAssetIds
    pure a
  AttachAsset aid _ | aid `member` investigatorAssets -> do
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  PlaceAsset aid placement | aid `member` investigatorAssets ->
    case placement of
      InPlayArea iid | iid == investigatorId -> pure a
      _ -> pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  PlaceAsset aid placement | aid `notMember` investigatorAssets -> do
    case placement of
      InPlayArea iid | iid == investigatorId -> do
        push $ InvestigatorPlayAsset iid aid
      _ -> pure ()
    pure a
  PlaceEvent _ eid placement | eid `member` investigatorEvents ->
    case placement of
      InPlayArea iid | iid == investigatorId -> pure a
      InThreatArea iid | iid == investigatorId -> pure a
      _ -> pure $ a & (eventsL %~ deleteSet eid)
  PlaceEvent _ eid placement | eid `notMember` investigatorEvents -> do
    case placement of
      InPlayArea iid | iid == investigatorId -> do
        pure $ a & (eventsL %~ insertSet eid)
      InThreatArea iid | iid == investigatorId -> do
        pure $ a & (eventsL %~ insertSet eid)
      _ -> pure a
  AttachTreachery tid (InvestigatorTarget iid) | iid == investigatorId -> do
    pure $ a & treacheriesL %~ insertSet tid
  PlaceKey (isTarget a -> True) k -> do
    pure $ a & keysL %~ insertSet k
  PlaceKey (isTarget a -> False) k -> do
    pure $ a & keysL %~ deleteSet k
  AllCheckHandSize | not (a ^. defeatedL || a ^. resignedL) -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    pushWhen (inHandCount > handSize) $ CheckHandSize investigatorId
    pure a
  CheckHandSize iid | iid == investigatorId -> do
    handSize <- getHandSize a
    inHandCount <- getInHandCount a
    pushWhen (inHandCount > handSize)
      $ chooseOne iid
      $ [ targetLabel (toCardId card) [DiscardCard iid GameSource (toCardId card), CheckHandSize iid]
        | card <- filter (isNothing . cdCardSubType . toCardDef) $ onlyPlayerCards investigatorHand
        ]
    pure a
  AddToDiscard iid pc | iid == investigatorId -> do
    pure $ a & discardL %~ (pc :) & (foundCardsL . each %~ filter (/= PlayerCard pc))
  DiscardFromHand handDiscard | discardInvestigator handDiscard == investigatorId -> do
    push $ DoneDiscarding investigatorId
    case discardStrategy handDiscard of
      DiscardChoose -> case discardableCards a of
        [] -> pure ()
        cs ->
          pushAll
            $ replicate (discardAmount handDiscard)
            $ chooseOrRunOne investigatorId
            $ [ targetLabel (toCardId c) [DiscardCard investigatorId (discardSource handDiscard) (toCardId c)]
              | c <- cs
              ]
      DiscardRandom -> do
        let filtered = filter (`cardMatch` (discardFilter handDiscard)) investigatorHand
        for_ (nonEmpty filtered) $ \targets -> do
          cards <- sampleN (discardAmount handDiscard) targets
          pushAll $ map (DiscardCard investigatorId (discardSource handDiscard) . toCardId) cards
    pure $ a & discardingL ?~ handDiscard
  Discard source (CardIdTarget cardId) | isJust (find ((== cardId) . toCardId) investigatorHand) -> do
    push (DiscardCard investigatorId source cardId)
    pure a
  Discard source (CardTarget card) | card `elem` investigatorHand -> do
    push $ DiscardCard investigatorId source (toCardId card)
    pure a
  DiscardHand iid source | iid == investigatorId -> do
    pushAll $ map (DiscardCard iid source . toCardId) investigatorHand
    pure a
  DiscardCard iid source cardId | iid == investigatorId -> do
    let card = fromJustNote "must be in hand" $ find ((== cardId) . toCardId) investigatorHand
    beforeWindowMsg <- checkWindows [mkWhen (Window.Discarded iid source card)]
    afterWindowMsg <- checkWindows [mkAfter (Window.Discarded iid source card)]
    pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
    pure a
  Do (DiscardCard iid _source cardId) | iid == investigatorId -> do
    let card = fromJustNote "must be in hand" $ find ((== cardId) . toCardId) investigatorHand
    case card of
      PlayerCard pc -> do
        let
          updateHandDiscard handDiscard =
            handDiscard
              { discardAmount = max 0 (discardAmount handDiscard - 1)
              }
        pure
          $ a
          & handL
          %~ filter (/= card)
          & discardL
          %~ (pc :)
          & discardingL
          %~ fmap updateHandDiscard
      EncounterCard _ -> pure $ a & handL %~ filter (/= card) -- TODO: This should discard to the encounter discard
      VengeanceCard _ -> error "vengeance card"
  DoneDiscarding iid | iid == investigatorId -> case investigatorDiscarding of
    Nothing -> pure a
    Just handDiscard -> do
      when (discardAmount handDiscard == 0)
        $ for_ (discardThen handDiscard) push
      pure $ a & discardingL .~ Nothing
  RemoveCardFromHand iid cardId | iid == investigatorId -> do
    pure $ a & handL %~ filter ((/= cardId) . toCardId)
  RemoveCardFromSearch iid cardId | iid == investigatorId -> do
    pure $ a & foundCardsL %~ Map.map (filter ((/= cardId) . toCardId))
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (TreacheryTarget tid) | iid == investigatorId -> do
    pure $ a & treacheriesL %~ deleteSet tid
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" . preview _PlayerCard <$> field AssetCard aid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    push $ After msg
    pure
      $ a
      & (assetsL %~ deleteSet aid)
      & (deckL .~ Deck deck')
      & (slotsL %~ removeFromSlots aid)
  ShuffleIntoDeck (Deck.InvestigatorDeck iid) (EventTarget eid) | iid == investigatorId -> do
    card <- fromJustNote "missing card" . preview _PlayerCard <$> field EventCard eid
    deck' <- shuffleM (card : unDeck investigatorDeck)
    push $ After msg
    pure $ a & (deckL .~ Deck deck')
  Discard _ (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
  Discard _ (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
  Discarded (EnemyTarget eid) _ _ ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  PlaceEnemy eid (InThreatArea iid') | iid' == investigatorId -> do
    pure $ a & engagedEnemiesL %~ insertSet eid
  PlaceEnemyInVoid eid -> pure $ a & engagedEnemiesL %~ deleteSet eid
  Discarded (AssetTarget aid) _ (PlayerCard card) | aid `elem` investigatorAssets -> do
    -- let
    --   slotTypes = cdSlots (toCardDef card)
    --   slots slotType = findWithDefault [] slotType investigatorSlots
    --   assetIds slotType = mapMaybe slotItem $ slots slotType
    push $ RefillSlots investigatorId
    pure $ a & (assetsL %~ deleteSet aid) & (discardL %~ (card :)) & (slotsL %~ removeFromSlots aid)
  Discarded (AssetTarget aid) _ (EncounterCard _) | aid `elem` investigatorAssets -> do
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  Exiled (AssetTarget aid) _ | aid `elem` investigatorAssets -> do
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (AssetTarget aid) ->
    pure $ a & (assetsL %~ deleteSet aid) & (slotsL %~ removeFromSlots aid)
  RemoveFromGame (CardIdTarget cid) ->
    pure $ a & cardsUnderneathL %~ filter ((/= cid) . toCardId)
  ChooseFightEnemy iid source mTarget skillType enemyMatcher isAction | iid == investigatorId -> do
    modifiers <- getModifiers (InvestigatorTarget iid)
    let
      isOverride = \case
        EnemyFightActionCriteria override -> Just override
        CanModify (EnemyFightActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride modifiers
      applyMatcherModifiers :: ModifierType -> EnemyMatcher -> EnemyMatcher
      applyMatcherModifiers (Modifier.AlternateFightField someField) original = case someField of
        SomeField Field.EnemyEvade -> original <> EnemyWithEvade
        _ -> original
      applyMatcherModifiers _ n = n
      canFightMatcher = case overrides of
        [] -> CanFightEnemy source
        [o] -> CanFightEnemyWithOverride o
        _ -> error "multiple overrides found"
    enemyIds <- selectList $ foldr applyMatcherModifiers (canFightMatcher <> enemyMatcher) modifiers
    push
      $ chooseOne
        iid
        [ FightLabel eid [FightEnemy iid eid source mTarget skillType isAction]
        | eid <- enemyIds
        ]
    pure a
  EngageEnemy iid eid True | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #engage)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #engage)]

    pushAll
      $ [ BeginAction
        , beforeWindowMsg
        , TakeAction iid (Just #engage) (ActionCost 1)
        ]
      <> [ CheckAttackOfOpportunity iid False
         | ActionDoesNotCauseAttacksOfOpportunity #engage `notElem` modifiers'
         ]
      <> [EngageEnemy iid eid False, afterWindowMsg, FinishAction]
    pure a
  EngageEnemy iid eid False | iid == investigatorId -> do
    pure $ a & engagedEnemiesL %~ insertSet eid
  EngageEnemy iid eid False | iid /= investigatorId -> do
    pure $ a & engagedEnemiesL %~ deleteSet eid
  FightEnemy iid eid source mTarget skillType True | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid #fight]
    afterWindowMsg <- checkWindows [mkAfter $ Window.PerformAction iid #fight]
    let
      performedActions = setFromList @(Set Action) investigatorActionsPerformed
      -- takenActions = setFromList @(Set Action) investigatorActionsTaken
      applyFightCostModifiers :: Cost -> ModifierType -> Cost
      applyFightCostModifiers costToEnter (ActionCostOf actionTarget n) =
        case actionTarget of
          FirstOneOfPerformed as
            | #fight `elem` as
            , null (performedActions `intersect` setFromList as) -> do
                increaseActionCost costToEnter n
          IsAction Action.Fight -> increaseActionCost costToEnter n
          _ -> costToEnter
      applyFightCostModifiers costToEnter _ = costToEnter
    pushAll
      [ BeginAction
      , beforeWindowMsg
      , TakeAction iid (Just #fight) (foldl' applyFightCostModifiers (ActionCost 1) modifiers')
      , FightEnemy iid eid source mTarget skillType False
      , afterWindowMsg
      , FinishAction
      ]
    pure a
  FightEnemy iid eid source mTarget skillType False | iid == investigatorId -> do
    a <$ push (AttackEnemy iid eid source mTarget skillType)
  FailedAttackEnemy iid eid | iid == investigatorId -> do
    doesNotDamageOtherInvestigators <- hasModifier a DoesNotDamageOtherInvestigator
    unless doesNotDamageOtherInvestigators $ do
      investigatorIds <- selectList $ InvestigatorEngagedWith $ EnemyWithId eid
      case investigatorIds of
        [x] | x /= iid -> push (InvestigatorDamageInvestigator iid x)
        _ -> pure ()
    pure a
  PlaceAdditionalDamage target source damage horror | isTarget a target -> do
    push $ Msg.InvestigatorDamage (toId a) source damage horror
    pure a
  InvestigatorDamageInvestigator iid xid | iid == investigatorId -> do
    damage <- damageValueFor 1 iid
    push $ InvestigatorAssignDamage xid (InvestigatorSource iid) DamageAny damage 0
    pure a
  InvestigatorDamageEnemy iid eid source | iid == investigatorId -> do
    damage <- damageValueFor 1 iid
    push $ EnemyDamage eid $ attack source damage
    pure a
  EnemyEvaded iid eid | iid == investigatorId -> do
    doNotDisengage <- hasModifier a DoNotDisengageEvaded
    push =<< checkWindows [mkAfter (Window.EnemyEvaded iid eid)]
    let
      updateEngagedEnemies =
        if doNotDisengage then id else engagedEnemiesL %~ deleteSet eid
    pure $ a & updateEngagedEnemies
  AddToVictory (EnemyTarget eid) -> pure $ a & engagedEnemiesL %~ deleteSet eid
  AddToVictory (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
  DefeatedAddToVictory (EnemyTarget eid) ->
    pure $ a & engagedEnemiesL %~ deleteSet eid
  ChooseEvadeEnemy iid source mTarget skillType enemyMatcher isAction | iid == investigatorId -> do
    modifiers <- getModifiers (InvestigatorTarget iid)
    let
      isOverride = \case
        EnemyEvadeActionCriteria override -> Just override
        CanModify (EnemyEvadeActionCriteria override) -> Just override
        _ -> Nothing
      overrides = mapMaybe isOverride modifiers
      applyMatcherModifiers :: ModifierType -> EnemyMatcher -> EnemyMatcher
      applyMatcherModifiers (Modifier.AlternateEvadeField someField) original = case someField of
        SomeField Field.EnemyEvade -> original <> EnemyWithEvade
        _ -> original
      applyMatcherModifiers _ n = n
      canEvadeMatcher = case overrides of
        [] -> CanEvadeEnemy source
        [o] -> CanEvadeEnemyWithOverride o
        _ -> error "multiple overrides found"
    enemyIds <- selectList $ foldr applyMatcherModifiers (canEvadeMatcher <> enemyMatcher) modifiers
    push
      $ chooseOne
        iid
        [ EvadeLabel eid [EvadeEnemy iid eid source mTarget skillType isAction]
        | eid <- enemyIds
        ]
    pure a
  EvadeEnemy iid eid source mTarget skillType True | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid #evade]
    afterWindowMsg <- checkWindows [mkAfter $ Window.PerformAction iid #evade]
    let
      performedActions = setFromList @(Set Action) investigatorActionsPerformed
      -- takenActions = setFromList @(Set Action) investigatorActionsTaken
      applyEvadeCostModifiers :: Cost -> ModifierType -> Cost
      applyEvadeCostModifiers costToEnter (ActionCostOf actionTarget n) =
        case actionTarget of
          FirstOneOfPerformed as
            | #evade `elem` as
            , null (performedActions `intersect` setFromList as) ->
                increaseActionCost costToEnter n
          IsAction Action.Evade -> increaseActionCost costToEnter n
          _ -> costToEnter
      applyEvadeCostModifiers costToEnter _ = costToEnter
    pushAll
      [ BeginAction
      , beforeWindowMsg
      , TakeAction iid (Just #evade) (foldl' applyEvadeCostModifiers (ActionCost 1) modifiers')
      , EvadeEnemy iid eid source mTarget skillType False
      , afterWindowMsg
      , FinishAction
      ]
    pure a
  EvadeEnemy iid eid source mTarget skillType False | iid == investigatorId -> do
    pushAll [TryEvadeEnemy iid eid source mTarget skillType, AfterEvadeEnemy iid eid]
    pure a
  MoveAction iid lid cost True | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #move)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #move)]
    pushAll
      [ BeginAction
      , beforeWindowMsg
      , TakeAction iid (Just #move) cost
      , MoveAction iid lid cost False
      , afterWindowMsg
      , FinishAction
      ]
    pure a
  MoveAction iid lid _cost False | iid == investigatorId -> do
    afterWindowMsg <-
      Helpers.checkWindows [mkAfter $ Window.MoveAction iid investigatorLocation lid]
    pushAll $ resolve (Move (move (toSource a) iid lid)) <> [afterWindowMsg]
    pure a
  Move movement | isTarget a (moveTarget movement) -> do
    case moveDestination movement of
      ToLocationMatching matcher -> do
        lids <- selectList matcher
        push
          $ chooseOrRunOne investigatorId
          $ [targetLabel lid [Move $ movement {moveDestination = ToLocation lid}] | lid <- lids]
      ToLocation destinationLocationId -> do
        let
          source = moveSource movement
          iid = investigatorId
        mFromLocation <- field InvestigatorLocation iid
        windowMsgs <-
          Helpers.windows
            [Window.Moves iid source mFromLocation destinationLocationId]
        pushAll
          $ [ Will (MoveFrom source iid fromLocationId)
            | fromLocationId <- maybeToList mFromLocation
            ]
          <> [Will (MoveTo $ move source iid destinationLocationId)]
          <> [ MoveFrom source iid fromLocationId
             | fromLocationId <- maybeToList mFromLocation
             ]
          <> [MoveTo $ move source iid destinationLocationId]
          <> windowMsgs
    pure a
  Will (PassedSkillTest iid _ _ (InvestigatorTarget iid') _ _) | iid == iid' && iid == investigatorId -> do
    pushM $ checkWindows [mkWhen (Window.WouldPassSkillTest iid)]
    pure a
  Will (FailedSkillTest iid _ _ (InvestigatorTarget iid') _ _) | iid == iid' && iid == toId a -> do
    pushM $ checkWindows [mkWhen (Window.WouldFailSkillTest iid)]
    pure a
  CancelDamage iid n | iid == investigatorId -> do
    withQueue_ \queue -> flip map queue $ \case
      Msg.InvestigatorDamage iid' s damage' horror' ->
        Msg.InvestigatorDamage iid' s (max 0 (damage' - n)) horror'
      InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
        InvestigatorDoAssignDamage iid' s t matcher' (max 0 (damage' - n)) horror' aa b
      other -> other
    pure a
  CancelHorror iid n | iid == investigatorId -> do
    withQueue_ \queue -> flip map queue $ \case
      Msg.InvestigatorDamage iid' s damage' horror' ->
        Msg.InvestigatorDamage iid' s damage' (max 0 (horror' - n))
      InvestigatorDoAssignDamage iid' s t matcher' damage' horror' aa b ->
        InvestigatorDoAssignDamage iid' s t matcher' damage' (max 0 (horror' - n)) aa b
      other -> other
    pure a
  InvestigatorDirectDamage iid source damage horror
    | iid == toId a
    , not (investigatorDefeated || investigatorResigned) -> do
        pushAll
          $ [ CheckWindow [iid]
              $ (mkWhen $ Window.WouldTakeDamageOrHorror source (toTarget a) damage horror)
              : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage) | damage > 0]
                <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror) | horror > 0]
            | damage > 0 || horror > 0
            ]
          <> [ InvestigatorDoAssignDamage
                iid
                source
                DamageAny
                (AssetWithModifier CanBeAssignedDirectDamage)
                damage
                horror
                []
                []
             , CheckDefeated source
             ]
        pure a
  InvestigatorAssignDamage iid source strategy damage horror
    | iid == toId a
    , not (investigatorDefeated || investigatorResigned) -> do
        modifiers <- getModifiers (toTarget a)
        if TreatAllDamageAsDirect `elem` modifiers
          then push $ InvestigatorDirectDamage iid source damage horror
          else
            pushAll
              $ [ CheckWindow [iid]
                  $ (mkWhen $ Window.WouldTakeDamageOrHorror source (toTarget a) damage horror)
                  : [mkWhen (Window.WouldTakeDamage source (toTarget a) damage) | damage > 0]
                    <> [mkWhen (Window.WouldTakeHorror source (toTarget a) horror) | horror > 0]
                | damage > 0 || horror > 0
                ]
              <> [ InvestigatorDoAssignDamage iid source strategy AnyAsset damage horror [] []
                 , CheckDefeated source
                 ]
        pure a
  InvestigatorDoAssignDamage iid source damageStrategy _ 0 0 damageTargets horrorTargets | iid == toId a -> do
    let
      damageEffect = case source of
        EnemyAttackSource _ -> AttackDamageEffect
        _ -> NonAttackDamageEffect
      damageMap = frequencies damageTargets
      horrorMap = frequencies horrorTargets
      placedWindows =
        [ Window.PlacedDamage source target damage
        | (target, damage) <- mapToList damageMap
        ]
          <> [ Window.PlacedHorror source target horror
             | (target, horror) <- mapToList horrorMap
             ]
    placedWindowMsg <-
      checkWindows $ concatMap (\t -> map (mkWindow t) placedWindows) [#when, #after]
    pushAll
      $ [ placedWindowMsg
        , CheckWindow [iid]
            $ [ mkWhen (Window.DealtDamage source damageEffect target damage)
              | target <- nub damageTargets
              , let damage = count (== target) damageTargets
              ]
            <> [ mkWhen (Window.DealtHorror source target horror)
               | target <- nub horrorTargets
               , let horror = count (== target) horrorTargets
               ]
            <> [mkWhen (Window.AssignedHorror source iid horrorTargets) | notNull horrorTargets]
        , CheckWindow [iid]
            $ [ mkAfter (Window.DealtDamage source damageEffect target damage)
              | target <- nub damageTargets
              , let damage = count (== target) damageTargets
              ]
            <> [ mkAfter (Window.DealtHorror source target horror)
               | target <- nub horrorTargets
               , let horror = count (== target) horrorTargets
               ]
            <> [mkAfter (Window.AssignedHorror source iid horrorTargets) | notNull horrorTargets]
        ]
    when
      ( damageStrategy
          == DamageFromHastur
          && toTarget a
          `elem` horrorTargets
          && investigatorSanityDamage a
          > investigatorSanity
      )
      $ push
      $ InvestigatorDirectDamage iid source 1 0
    pure a
  InvestigatorDoAssignDamage iid source DamageEvenly matcher health 0 damageTargets horrorTargets | iid == toId a -> do
    healthDamageableAssets <-
      toList
        <$> getHealthDamageableAssets
          iid
          matcher
          health
          damageTargets
          horrorTargets
    let
      getDamageTargets xs =
        if length xs >= length healthDamageableAssets + 1
          then getDamageTargets (drop (length healthDamageableAssets + 1) xs)
          else xs
      damageTargets' = getDamageTargets damageTargets
      healthDamageableAssets' =
        filter
          ((`notElem` damageTargets') . AssetTarget)
          healthDamageableAssets
      assignRestOfHealthDamage =
        InvestigatorDoAssignDamage
          investigatorId
          source
          DamageEvenly
          matcher
          (health - 1)
          0
      -- N.B. we have to add to the end of targets to handle the drop logic
      damageAsset aid =
        ComponentLabel
          (AssetComponent aid DamageToken)
          [ Msg.AssetDamage aid source 1 0
          , assignRestOfHealthDamage (damageTargets <> [AssetTarget aid]) mempty
          ]
      damageInvestigator =
        ComponentLabel
          (InvestigatorComponent investigatorId DamageToken)
          [ Msg.InvestigatorDamage investigatorId source 1 0
          , assignRestOfHealthDamage
              (damageTargets <> [InvestigatorTarget investigatorId])
              mempty
          ]
      healthDamageMessages =
        [ damageInvestigator
        | InvestigatorTarget investigatorId `notElem` damageTargets'
        ]
          <> map damageAsset healthDamageableAssets'
    push $ chooseOne iid healthDamageMessages
    pure a
  InvestigatorDoAssignDamage iid source DamageEvenly matcher 0 sanity damageTargets horrorTargets | iid == toId a -> do
    sanityDamageableAssets <-
      toList
        <$> getSanityDamageableAssets
          iid
          matcher
          sanity
          damageTargets
          horrorTargets
    mustBeDamagedFirstBeforeInvestigator <-
      selectList
        ( AssetCanBeAssignedHorrorBy iid
            <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
        )
    let
      getDamageTargets xs =
        if length xs >= length sanityDamageableAssets + 1
          then getDamageTargets (drop (length sanityDamageableAssets + 1) xs)
          else xs
      horrorTargets' = getDamageTargets horrorTargets
      sanityDamageableAssets' =
        filter
          ((`notElem` horrorTargets') . AssetTarget)
          sanityDamageableAssets
      assignRestOfSanityDamage =
        InvestigatorDoAssignDamage
          investigatorId
          source
          DamageEvenly
          matcher
          0
          (sanity - 1)
      -- N.B. we have to add to the end of targets to handle the drop logic
      damageAsset aid =
        ComponentLabel
          (AssetComponent aid HorrorToken)
          [ Msg.AssetDamage aid source 0 1
          , assignRestOfSanityDamage mempty (horrorTargets <> [AssetTarget aid])
          ]
      damageInvestigator =
        ComponentLabel
          (InvestigatorComponent investigatorId HorrorToken)
          [ Msg.InvestigatorDamage investigatorId source 0 1
          , assignRestOfSanityDamage
              mempty
              (horrorTargets <> [InvestigatorTarget investigatorId])
          ]
      sanityDamageMessages =
        [ damageInvestigator
        | InvestigatorTarget investigatorId
            `notElem` horrorTargets'
            && null mustBeDamagedFirstBeforeInvestigator
        ]
          <> map damageAsset sanityDamageableAssets'
    push $ chooseOne iid sanityDamageMessages
    pure a
  InvestigatorDoAssignDamage iid _ DamageEvenly _ _ _ _ _ | iid == investigatorId -> do
    error "DamageEvenly only works with just horror or just damage, but not both"
  InvestigatorDoAssignDamage iid source SingleTarget matcher health sanity damageTargets horrorTargets | iid == toId a -> do
    healthDamageableAssets <-
      getHealthDamageableAssets
        iid
        matcher
        health
        damageTargets
        horrorTargets
    sanityDamageableAssets <-
      getSanityDamageableAssets
        iid
        matcher
        sanity
        damageTargets
        horrorTargets
    let
      damageableAssets =
        toList $ healthDamageableAssets `union` sanityDamageableAssets
      continue h s t =
        InvestigatorDoAssignDamage
          iid
          source
          SingleTarget
          matcher
          (max 0 $ health - h)
          (max 0 $ sanity - s)
          (damageTargets <> [t | h > 0])
          (horrorTargets <> [t | s > 0])
      toAssetMessage (asset, (h, s)) =
        TargetLabel
          (AssetTarget asset)
          [ Msg.AssetDamage asset source (min h health) (min s sanity)
          , continue h s (AssetTarget asset)
          ]
    assetsWithCounts <- for damageableAssets $ \asset -> do
      health' <- fieldMap AssetRemainingHealth (fromMaybe 0) asset
      sanity' <- fieldMap AssetRemainingSanity (fromMaybe 0) asset
      pure (asset, (health', sanity'))

    push
      $ chooseOne iid
      $ targetLabel
        a
        [ Msg.InvestigatorDamage investigatorId source health sanity
        , continue health sanity (toTarget a)
        ]
      : map toAssetMessage assetsWithCounts
    pure a
  InvestigatorDoAssignDamage iid source strategy matcher health sanity damageTargets horrorTargets | iid == toId a -> do
    healthDamageMessages <-
      if health > 0
        then do
          healthDamageableAssets <-
            toList <$> getHealthDamageableAssets iid matcher health damageTargets horrorTargets
          let
            assignRestOfHealthDamage =
              InvestigatorDoAssignDamage investigatorId source strategy matcher (health - 1) sanity
            damageAsset aid =
              ComponentLabel
                (AssetComponent aid DamageToken)
                [ Msg.AssetDamage aid source 1 0
                , assignRestOfHealthDamage
                    (AssetTarget aid : damageTargets)
                    horrorTargets
                ]
            damageInvestigator =
              ComponentLabel
                (InvestigatorComponent investigatorId DamageToken)
                [ Msg.InvestigatorDamage investigatorId source 1 0
                , assignRestOfHealthDamage (toTarget investigatorId : damageTargets) horrorTargets
                ]
          case strategy of
            DamageAssetsFirst -> do
              pure
                $ [damageInvestigator | null healthDamageableAssets]
                <> map damageAsset healthDamageableAssets
            DamageAny ->
              pure $ damageInvestigator : map damageAsset healthDamageableAssets
            DamageFromHastur ->
              pure $ damageInvestigator : map damageAsset healthDamageableAssets
            DamageFirst def -> do
              validAssets <-
                setToList
                  . intersection (setFromList healthDamageableAssets)
                  <$> select (matcher <> AssetControlledBy You <> assetIs def)
              pure
                $ if null validAssets
                  then damageInvestigator : map damageAsset healthDamageableAssets
                  else map damageAsset validAssets
            SingleTarget -> error "handled elsewhere"
            DamageEvenly -> error "handled elsewhere"
        else pure []
    sanityDamageMessages <-
      if sanity > 0
        then do
          sanityDamageableAssets <-
            toList
              <$> getSanityDamageableAssets iid matcher sanity damageTargets horrorTargets
          let
            assignRestOfSanityDamage =
              InvestigatorDoAssignDamage investigatorId source strategy matcher health (sanity - 1)
            damageInvestigator =
              ComponentLabel
                (InvestigatorComponent investigatorId HorrorToken)
                [ Msg.InvestigatorDamage investigatorId source 0 1
                , assignRestOfSanityDamage damageTargets (toTarget investigatorId : horrorTargets)
                ]
            damageAsset aid =
              ComponentLabel
                (AssetComponent aid HorrorToken)
                [ Msg.AssetDamage aid source 0 1
                , assignRestOfSanityDamage damageTargets (toTarget aid : horrorTargets)
                ]
          case strategy of
            DamageAssetsFirst ->
              pure
                $ [damageInvestigator | null sanityDamageableAssets]
                <> map damageAsset sanityDamageableAssets
            DamageAny -> do
              mustBeDamagedFirstBeforeInvestigator <-
                selectList
                  ( AssetCanBeAssignedHorrorBy iid
                      <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
                  )
              pure
                $ [ damageInvestigator
                  | null mustBeDamagedFirstBeforeInvestigator
                  ]
                <> map damageAsset sanityDamageableAssets
            DamageFromHastur -> do
              mustBeDamagedFirstBeforeInvestigator <-
                selectList
                  ( AssetCanBeAssignedHorrorBy iid
                      <> AssetWithModifier NonDirectHorrorMustBeAssignToThisFirst
                  )
              pure
                $ [ damageInvestigator
                  | null mustBeDamagedFirstBeforeInvestigator
                  ]
                <> map damageAsset sanityDamageableAssets
            DamageFirst def -> do
              validAssets <-
                setToList
                  . intersection (setFromList sanityDamageableAssets)
                  <$> select (matcher <> AssetControlledBy You <> assetIs def)
              pure
                $ if null validAssets
                  then damageInvestigator : map damageAsset sanityDamageableAssets
                  else map damageAsset validAssets
            SingleTarget -> error "handled elsewhere"
            DamageEvenly -> error "handled elsewhere"
        else pure []
    push $ chooseOne iid $ healthDamageMessages <> sanityDamageMessages
    pure a
  Investigate iid lid source mTarget skillType True | iid == investigatorId ->
    do
      beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #investigate)]
      afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #investigate)]
      modifiers <- getModifiers (LocationTarget lid)
      modifiers' <- getModifiers (toTarget a)
      let
        investigateCost = foldr applyModifier 1 modifiers
        applyModifier (ActionCostOf (IsAction Action.Investigate) m) n = max 0 (n + m)
        applyModifier _ n = n
      pushAll
        $ [ BeginAction
          , beforeWindowMsg
          , TakeAction iid (Just #investigate) (ActionCost investigateCost)
          ]
        <> [ CheckAttackOfOpportunity iid False
           | ActionDoesNotCauseAttacksOfOpportunity #investigate `notElem` modifiers'
           ]
        <> [ Investigate iid lid source mTarget skillType False
           , afterWindowMsg
           , FinishAction
           ]
      pure a
  InvestigatorDiscoverCluesAtTheirLocation iid source n maction | iid == investigatorId -> do
    runMessage (InvestigatorDiscoverClues iid investigatorLocation source n maction) a
  InvestigatorDiscoverClues iid lid source n _ | iid == investigatorId -> do
    canDiscoverClues <- getCanDiscoverClues a lid
    when canDiscoverClues $ do
      checkWindowMsg <- checkWindows [mkWhen (Window.DiscoverClues iid lid source n)]
      pushAll [checkWindowMsg, Do msg]
    pure a
  Do (InvestigatorDiscoverClues iid lid source n maction) | iid == investigatorId -> do
    canDiscoverClues <- getCanDiscoverClues a lid
    pushWhen canDiscoverClues $ DiscoverCluesAtLocation iid lid source n maction
    pure a
  GainClues iid source n | iid == investigatorId -> do
    window <- checkWindows ((`mkWindow` Window.GainsClues iid source n) <$> [#when, #after])
    pushAll [window, PlaceTokens source (toTarget iid) Clue n, After (GainClues iid source n)]
    pure a
  FlipClues target n | isTarget a target -> do
    pure $ a & tokensL %~ flipClues n
  DiscoverClues iid lid source n maction | iid == investigatorId -> do
    modifiers <- getModifiers lid
    let
      getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
      getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
      getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
      getMaybeMax _ x = x
      mMax :: Maybe Int = foldr getMaybeMax Nothing modifiers
      n' = maybe n (min n) mMax
    push $ Do $ DiscoverClues iid lid source n' maction
    pure a
  Do (DiscoverClues iid _ source n _) | iid == investigatorId -> do
    send $ format a <> " discovered " <> pluralize n "clue"
    push $ After $ GainClues iid source n
    pure $ a & tokensL %~ addTokens Clue n
  InvestigatorDiscardAllClues _ iid | iid == investigatorId -> do
    pure $ a & tokensL %~ removeAllTokens Clue
  MoveAllCluesTo source target | not (isTarget a target) -> do
    when (investigatorClues a > 0) (push $ PlaceTokens source target Clue $ investigatorClues a)
    pure $ a & tokensL %~ removeAllTokens Clue
  InitiatePlayCardAsChoose iid card choices msgs chosenCardStrategy windows' asAction | iid == toId a -> do
    event <- selectJust $ EventWithCardId $ toCardId card
    push
      $ chooseOne iid
      $ [ targetLabel (toCardId choice)
          $ [ ReturnToHand iid (toTarget event)
            , InitiatePlayCardAs iid card choice msgs chosenCardStrategy windows' asAction
            ]
        | choice <- choices
        ]
    pure a
  InitiatePlayCardAs iid card choice msgs chosenCardStrategy windows' asAction | iid == toId a -> do
    let
      choiceDef = toCardDef choice
      choiceAsCard =
        (lookupPlayerCard choiceDef $ toCardId card)
          { pcOriginalCardCode = toCardCode card
          }
      chosenCardMsgs = case chosenCardStrategy of
        LeaveChosenCard -> []
        RemoveChosenCardFromGame -> [RemovePlayerCardFromGame True choice]

    pushAll
      $ chosenCardMsgs
      <> msgs
      <> [InitiatePlayCard iid (PlayerCard choiceAsCard) Nothing windows' asAction]
    pure $ a & handL %~ (PlayerCard choiceAsCard :) . filter (/= card)
  InitiatePlayCard iid card mtarget windows' asAction | iid == investigatorId ->
    do
      -- we need to check if the card is first an AsIfInHand card, if it is, then we let the owning entity handle this message
      modifiers' <- getModifiers (toTarget a)
      let
        shouldSkip = flip any modifiers' $ \case
          AsIfInHand card' -> card == card'
          _ -> False
      unless shouldSkip $ do
        afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid card)]

        pushAll
          [ CheckWindow [iid] [mkWhen (Window.PlayCard iid card)]
          , PlayCard iid card mtarget windows' asAction
          , afterPlayCard
          ]
      pure a
  CardEnteredPlay iid card | iid == investigatorId -> do
    send $ format a <> " played " <> format card
    pure
      $ a
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
  ObtainCard card -> do
    pure
      $ a
      & (handL %~ filter (/= card))
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
  PutCampaignCardIntoPlay iid cardDef -> do
    let mcard = find ((== cardDef) . toCardDef) (unDeck investigatorDeck)
    case mcard of
      Nothing -> error "did not have campaign card"
      Just card -> push $ PutCardIntoPlay iid (PlayerCard card) Nothing []
    pure a
  InvestigatorPlayAsset iid aid | iid == investigatorId -> do
    -- It might seem weird that we remove the asset from the slots here since
    -- we haven't added it yet, however in the case that an asset adds some
    -- additional slot we'll have called RefillSlots already, and then this
    -- will have taken up the necessary slot so we remove it here so it can be
    -- placed correctly later
    pushAll [InvestigatorClearUnusedAssetSlots iid, Do (InvestigatorPlayAsset iid aid)]
    pure $ a & slotsL %~ removeFromSlots aid
  InvestigatorClearUnusedAssetSlots iid | iid == investigatorId -> do
    updatedSlots <- for (mapToList investigatorSlots) $ \(slotType, slots) -> do
      slots' <- for slots $ \slot -> do
        case slotItem slot of
          Nothing -> pure slot
          Just aid -> do
            ignored <- hasModifier (AssetTarget aid) (DoNotTakeUpSlot slotType)
            pure $ if ignored then emptySlot slot else slot
      pure (slotType, slots')
    pure $ a & slotsL .~ mapFromList updatedSlots
  Do (InvestigatorPlayAsset iid aid) | iid == investigatorId -> do
    -- this asset might already be slotted so check first
    fitsSlots <- fitsAvailableSlots aid a
    case fitsSlots of
      FitsSlots -> push (InvestigatorPlayedAsset iid aid)
      MissingSlots missingSlotTypes -> do
        assetsThatCanProvideSlots <-
          selectList
            $ assetControlledBy iid
            <> DiscardableAsset
            <> AssetOneOf (map AssetInSlot missingSlotTypes)
        push
          $ if null assetsThatCanProvideSlots
            then InvestigatorPlayedAsset iid aid
            else
              chooseOne iid
                $ [ targetLabel
                    aid'
                    [ Discard GameSource (AssetTarget aid')
                    , InvestigatorPlayAsset iid aid
                    ]
                  | aid' <- assetsThatCanProvideSlots
                  ]
    pure a
  InvestigatorPlayEvent iid eid _ _ _ | iid == investigatorId -> do
    pure $ a & eventsL %~ insertSet eid
  InvestigatorPlayedAsset iid aid | iid == investigatorId -> do
    let assetsUpdate = assetsL %~ insertSet aid
    slotTypes <- field AssetSlots aid
    assetCard <- field AssetCard aid

    canHoldMap :: Map SlotType [SlotType] <- do
      mods <- getModifiers a
      let
        canHold = \case
          SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
          _ -> id
      pure $ foldr canHold mempty mods
    -- we need to figure out which slots are or aren't available
    -- we've claimed we can play this, but we might need to change the slotType
    let
      handleSlotType :: Map SlotType [Slot] -> SlotType -> Map SlotType [Slot]
      handleSlotType slots' sType =
        case nub (availableSlotTypesFor sType canHoldMap assetCard a) of
          [] -> error "No slot found"
          [sType'] -> slots' & ix sType' %~ placeInAvailableSlot aid assetCard
          xs | sType `elem` xs -> slots' & ix sType %~ placeInAvailableSlot aid assetCard
          _ -> error $ "Too many slots found, we expect at max two and one must be the original slot"

      slots = foldl' handleSlotType (a ^. slotsL) slotTypes

    pure $ a & handL %~ (filter (/= assetCard)) & assetsUpdate & slotsL .~ slots
  RemoveCampaignCard cardDef -> do
    pure
      $ a
      & (deckL %~ Deck . filter ((/= toCardCode cardDef) . toCardCode) . unDeck)
  RemoveAllCopiesOfCardFromGame iid cardCode | iid == investigatorId -> do
    for_ investigatorAssets $ \assetId -> do
      cardCode' <- field AssetCardCode assetId
      when (cardCode == cardCode') (push $ RemoveFromGame (AssetTarget assetId))
    pure
      $ a
      & (deckL %~ Deck . filter ((/= cardCode) . toCardCode) . unDeck)
      & (discardL %~ filter ((/= cardCode) . toCardCode))
      & (handL %~ filter ((/= cardCode) . toCardCode))
  PutCardIntoPlay _ card _ _ -> do
    pure
      $ a
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
      & (discardL %~ filter ((/= card) . PlayerCard))
      & (handL %~ filter (/= card))
  Msg.InvestigatorDamage iid _ damage horror | iid == investigatorId -> do
    pure $ a & assignedHealthDamageL +~ damage & assignedSanityDamageL +~ horror
  DrivenInsane iid | iid == investigatorId -> do
    pure $ a & mentalTraumaL .~ investigatorSanity
  CheckDefeated source -> do
    facingDefeat <- getFacingDefeat a
    if facingDefeat
      then do
        modifiedHealth <- getModifiedHealth a
        modifiedSanity <- getModifiedSanity a
        let
          defeatedByHorror =
            investigatorSanityDamage a
              + investigatorAssignedSanityDamage
              >= modifiedSanity
          defeatedByDamage =
            investigatorHealthDamage a
              + investigatorAssignedHealthDamage
              >= modifiedHealth
          defeatedBy = case (defeatedByHorror, defeatedByDamage) of
            (True, True) -> DefeatedByDamageAndHorror source
            (True, False) -> DefeatedByHorror source
            (False, True) -> DefeatedByDamage source
            (False, False) -> DefeatedByOther source
        windowMsg <- checkWindows [mkWhen $ Window.InvestigatorWouldBeDefeated defeatedBy (toId a)]
        pushAll
          [ windowMsg
          , AssignDamage (InvestigatorTarget $ toId a)
          , InvestigatorWhenDefeated source investigatorId
          ]
      else push $ AssignDamage (InvestigatorTarget $ toId a)
    pure a
  AssignDamage target | isTarget a target -> do
    pure
      $ a
      & tokensL
      %~ ( addTokens Token.Damage investigatorAssignedHealthDamage
            . addTokens Horror investigatorAssignedSanityDamage
         )
      & (assignedHealthDamageL .~ 0)
      & (assignedSanityDamageL .~ 0)
  CancelAssignedDamage target damageReduction horrorReduction | isTarget a target -> do
    pure
      $ a
      & (assignedHealthDamageL %~ max 0 . subtract damageReduction)
      & (assignedSanityDamageL %~ max 0 . subtract horrorReduction)
  HealDamage (InvestigatorTarget iid) source amount | iid == investigatorId -> do
    afterWindow <- checkWindows [mkAfter $ Window.Healed DamageType (toTarget a) source amount]
    push afterWindow
    pure $ a & tokensL %~ subtractTokens Token.Damage amount
  HealHorrorWithAdditional (InvestigatorTarget iid) _ amount | iid == investigatorId -> do
    -- exists to have no callbacks, and to be resolved with AdditionalHealHorror
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure a
      else do
        pure
          $ a
          & (tokensL %~ subtractTokens Horror amount)
          & (horrorHealedL .~ (min amount $ investigatorSanityDamage a))
  AdditionalHealHorror (InvestigatorTarget iid) source additional | iid == investigatorId -> do
    -- exists to have Callbacks for the total, get from investigatorHorrorHealed
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure $ a & horrorHealedL .~ 0
      else do
        afterWindow <-
          checkWindows
            [ mkAfter
                $ Window.Healed HorrorType (toTarget a) source (investigatorHorrorHealed + additional)
            ]
        push afterWindow
        pure
          $ a
          & tokensL
          %~ subtractTokens Horror additional
          & horrorHealedL
          .~ 0
  HealHorror (InvestigatorTarget iid) source amount | iid == investigatorId -> do
    cannotHealHorror <- hasModifier a CannotHealHorror
    if cannotHealHorror
      then pure a
      else do
        afterWindow <- checkWindows [mkAfter $ Window.Healed HorrorType (toTarget a) source amount]
        push afterWindow
        pure $ a & tokensL %~ subtractTokens Horror amount
  MovedHorror _ (isTarget a -> True) amount -> do
    pure $ a & tokensL %~ addTokens Horror amount
  MovedHorror (isSource a -> True) _ amount -> do
    pure $ a & tokensL %~ subtractTokens Horror amount
  HealHorrorDirectly (InvestigatorTarget iid) _ amount | iid == investigatorId -> do
    -- USE ONLY WHEN NO CALLBACKS
    pure $ a & tokensL %~ subtractTokens Horror amount
  HealDamageDirectly (InvestigatorTarget iid) _ amount | iid == investigatorId -> do
    -- USE ONLY WHEN NO CALLBACKS
    pure $ a & tokensL %~ subtractTokens Token.Damage amount
  InvestigatorWhenDefeated source iid | iid == investigatorId -> do
    modifiedHealth <- getModifiedHealth a
    modifiedSanity <- getModifiedSanity a
    let
      defeatedByHorror = investigatorSanityDamage a >= modifiedSanity
      defeatedByDamage = investigatorHealthDamage a >= modifiedHealth
      defeatedBy = case (defeatedByHorror, defeatedByDamage) of
        (True, True) -> DefeatedByDamageAndHorror source
        (True, False) -> DefeatedByHorror source
        (False, True) -> DefeatedByDamage source
        (False, False) -> DefeatedByOther source
    windowMsg <- checkWindows [mkWhen $ Window.InvestigatorDefeated defeatedBy iid]
    pushAll [windowMsg, InvestigatorIsDefeated source iid]
    pure a
  InvestigatorKilled source iid | iid == investigatorId -> do
    unless investigatorDefeated $ do
      isLead <- (== iid) <$> getLeadInvestigatorId
      pushAll $ [ChooseLeadInvestigator | isLead] <> [Msg.InvestigatorDefeated source iid]
    pure $ a & defeatedL .~ True & endedTurnL .~ True
  MoveAllTo source lid | not (a ^. defeatedL || a ^. resignedL) -> do
    a <$ push (MoveTo $ move source investigatorId lid)
  MoveTo movement | isTarget a (moveTarget movement) -> do
    case moveDestination movement of
      ToLocationMatching matcher -> do
        lids <- selectList matcher
        push
          $ chooseOrRunOne
            investigatorId
            [targetLabel lid [MoveTo $ movement {moveDestination = ToLocation lid}] | lid <- lids]
        pure a
      ToLocation lid -> do
        let
          source = moveSource movement
          iid = investigatorId
        movedByWindows <- Helpers.windows [Window.MovedBy source lid iid]
        afterMoveButBeforeEnemyEngagement <-
          Helpers.checkWindows [mkAfter (Window.MovedButBeforeEnemyEngagement iid lid)]
        afterEnterWindow <- checkWindows [mkAfter (Window.Entering iid lid)]
        pushAll
          $ movedByWindows
          <> [ WhenWillEnterLocation iid lid
             , EnterLocation iid lid
             , afterEnterWindow
             , afterMoveButBeforeEnemyEngagement
             , CheckEnemyEngagement iid
             ]
        pure $ a & locationL .~ lid
  CheckEnemyEngagement iid | iid == investigatorId -> do
    enemies <- selectList $ EnemyAt $ LocationWithId investigatorLocation
    a <$ pushAll [EnemyCheckEngagement eid | eid <- enemies]
  SetLocationAsIf iid lid | iid == investigatorId -> do
    -- In the as if situation we want to avoid callbacks
    -- so this sets the value directly
    pure $ a & locationL .~ lid
  AddSlot iid slotType slot | iid == investigatorId -> do
    let
      slots = findWithDefault [] slotType investigatorSlots
      emptiedSlots = sort $ slot : map emptySlot slots
    push $ RefillSlots iid
    pure $ a & slotsL %~ insertMap slotType emptiedSlots
  RemoveSlot iid slotType | iid == investigatorId -> do
    -- This arbitrarily removes the first slot of the given type provided that
    -- it is granted by the investigator
    push $ RefillSlots iid
    pure $ a & slotsL . ix slotType %~ deleteFirstMatch (isSource a . slotSource)
  RefillSlots iid | iid == investigatorId -> do
    assetIds <- selectList $ assetControlledBy (toId a)

    requirements <- concatForM assetIds $ \assetId -> do
      assetCard <- field AssetCard assetId
      mods <- getModifiers assetId
      let slotFilter sType = DoNotTakeUpSlot sType `notElem` mods
      pure $ (assetId,assetCard,) <$> filter slotFilter (cdSlots $ toCardDef assetCard)

    let allSlots :: [(SlotType, Slot)] = concatMap (\(k, vs) -> (k,) . emptySlot <$> vs) $ Map.assocs (a ^. slotsL)

    canHoldMap :: Map SlotType [SlotType] <- do
      mods <- getModifiers a
      let
        canHold = \case
          SlotCanBe slotType canBeSlotType -> insertWith (<>) slotType [canBeSlotType]
          _ -> id
      pure $ foldr canHold mempty mods

    let
      go [] _ = []
      go rs [] = map (\(_, _, sType) -> sType) rs
      go ((_, card, slotType) : rs) slots = do
        case sort (filterBy [canPutIntoSlot card . snd, (== slotType) . fst] slots) of
          [] -> case findWithDefault [] slotType canHoldMap of
            [] -> slotType : go rs slots
            [other] ->
              case sort (filterBy [canPutIntoSlot card . snd, (== other) . fst] slots) of
                [] -> slotType : go rs slots
                (x : _) -> go rs (delete x slots)
            _ -> error "not designed to work with more than one yet"
          (x : _) -> go rs (delete x slots)

    let
      fill :: [(AssetId, Card, SlotType)] -> Map SlotType [Slot] -> Map SlotType [Slot]
      fill [] slots = slots
      fill ((aid, card, slotType) : rs) slots = do
        case sort (filter (canPutIntoSlot card) (slots ^. at slotType . non [])) of
          [] -> case findWithDefault [] slotType canHoldMap of
            [] -> error "can't be filled 1"
            [other] ->
              case sort (filter (canPutIntoSlot card) (slots ^. at other . non [])) of
                [] -> error "can't be filled 2"
                _ -> fill rs (slots & ix other %~ placeInAvailableSlot aid card)
            _ -> error "not designed to work with more than one yet"
          _ -> fill rs (slots & ix slotType %~ placeInAvailableSlot aid card)

    let
      failedSlotTypes = nub (go requirements allSlots)
      failedSlotTypes' = nub $ concatMap (\s -> s : findWithDefault [] s canHoldMap) failedSlotTypes
      failedAssetIds' = map (\(aid, _, _) -> aid) $ filter (\(_, _, s) -> s `elem` failedSlotTypes') requirements

    failedAssetIds <- selectFilter AssetCanLeavePlayByNormalMeans failedAssetIds'

    if null failedAssetIds
      then do
        pure $ a & slotsL %~ fill requirements . Map.map (map emptySlot)
      else do
        push
          $ chooseOne iid
          $ [ targetLabel aid' [Discard GameSource (AssetTarget aid'), RefillSlots iid]
            | aid' <- failedAssetIds
            ]
        pure a
  ChooseEndTurn iid | iid == investigatorId -> pure $ a & endedTurnL .~ True
  Do BeginRound -> do
    actionsForTurn <- getAbilitiesForTurn a
    modifiers' <- getModifiers (toTarget a)
    let
      toAdditionalAction = \case
        GiveAdditionalAction ac -> Just ac
        _ -> Nothing
      additionalActions = mapMaybe toAdditionalAction modifiers'
    pure
      $ a
      & (endedTurnL .~ False)
      & (remainingActionsL .~ actionsForTurn)
      & (additionalActionsL %~ (additionalActions <>))
      & (actionsTakenL .~ mempty)
  DiscardTopOfDeck iid n source mTarget | iid == investigatorId -> do
    let (cs, deck') = draw n investigatorDeck
    windowMsgs <-
      if null deck'
        then
          pure
            <$> checkWindows
              ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
        else pure []
    pushAll
      $ windowMsgs
      <> [DeckHasNoCards investigatorId mTarget | null deck']
      <> [ DiscardedTopOfDeck iid cs source target
         | target <- maybeToList mTarget
         ]
    pure $ a & deckL .~ deck' & discardL %~ (reverse cs <>)
  DiscardUntilFirst iid' source (Deck.InvestigatorDeck iid) matcher | iid == investigatorId -> do
    (discards, remainingDeck) <- breakM (`extendedCardMatch` matcher) (unDeck investigatorDeck)
    case remainingDeck of
      [] -> do
        pushAll [RequestedPlayerCard iid' source Nothing discards, DeckHasNoCards iid Nothing]
        pure $ a & deckL .~ mempty & discardL %~ (reverse discards <>)
      (x : xs) -> do
        push (RequestedPlayerCard iid' source (Just x) discards)
        pure $ a & deckL .~ Deck xs & discardL %~ (reverse discards <>)
  RevealUntilFirst iid source (Deck.InvestigatorDeck iid') matcher | iid == investigatorId && iid' == iid -> do
    let (revealed, remainingDeck) = break (`cardMatch` matcher) (unDeck investigatorDeck)
    case remainingDeck of
      [] -> do
        pushAll
          [ RevealedCards iid source (Deck.InvestigatorDeck iid') Nothing (map PlayerCard revealed)
          , DeckHasNoCards iid Nothing
          ]
        pure $ a & deckL .~ mempty
      (x : xs) -> do
        push
          $ RevealedCards
            iid
            source
            (Deck.InvestigatorDeck iid')
            (Just $ PlayerCard x)
            (map PlayerCard revealed)
        pure $ a & deckL .~ Deck xs
  DrawCards cardDraw | cardDrawInvestigator cardDraw == toId a && cardDrawAction cardDraw -> do
    let
      iid = investigatorId
      source = cardDrawSource cardDraw
      n = cardDrawAmount cardDraw
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #draw)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #draw)]
    drawing <- drawCards iid source n
    pushAll
      [ BeginAction
      , beforeWindowMsg
      , TakeAction iid (Just #draw) (ActionCost 1)
      , CheckAttackOfOpportunity iid False
      , drawing
      , afterWindowMsg
      , FinishAction
      ]
    pure a
  MoveTopOfDeckToBottom _ (Deck.InvestigatorDeck iid) n | iid == investigatorId -> do
    let (cards, deck) = draw n investigatorDeck
    pure $ a & deckL .~ Deck.withDeck (<> cards) deck
  DrawCards cardDraw | cardDrawInvestigator cardDraw == toId a && not (cardDrawAction cardDraw) -> do
    -- RULES: When a player draws two or more cards as the result of a single
    -- ability or game step, those cards are drawn simultaneously. If a deck
    -- empties middraw, reset the deck and complete the draw.

    -- RULES: If an investigator with an empty investigator deck needs to draw
    -- a card, that investigator shuffles his or her discard pile back into his
    -- or her deck, then draws the card, and upon completion of the entire draw
    -- takes one horror.

    let
      iid = investigatorId
      source = cardDrawSource cardDraw
      n = cardDrawAmount cardDraw

    modifiers' <- getModifiers (toTarget a)
    if null investigatorDeck
      then do
        -- What happens if the Yorick player has Graveyard Ghouls engaged
        -- with him or her and runs out of deck? "If an investigator with an
        -- empty investigator deck needs to draw a card, that investigator
        -- shuffles his or her discard pile back into his or her deck, then
        -- draws the card, and upon completion of the entire draw takes one
        -- horror. In this case, you cannot shuffle your discard pile into
        -- your deck, so you will neither draw 1 card, nor will you take 1
        -- horror."
        unless (null investigatorDiscard || CardsCannotLeaveYourDiscardPile `elem` modifiers') $ do
          drawing <- drawCards iid source n
          wouldDo
            (EmptyDeck iid (Just drawing))
            (Window.DeckWouldRunOutOfCards iid)
            (Window.DeckRanOutOfCards iid)
        -- push $ EmptyDeck iid
        pure a
      else do
        let deck = unDeck investigatorDeck
        if length deck < n
          then do
            drawing <- drawCards iid source (n - length deck)
            push drawing
            pure $ a & deckL .~ mempty & drawnCardsL %~ (<> deck)
          else do
            let
              (drawn, deck') = splitAt n deck
              allDrawn = investigatorDrawnCards <> drawn
              shuffleBackInEachWeakness =
                ShuffleBackInEachWeakness `elem` cardDrawRules cardDraw

              msgs = flip concatMap allDrawn $ \card ->
                case toCardType card of
                  PlayerTreacheryType ->
                    guard (not shuffleBackInEachWeakness)
                      *> [ DrewTreachery iid (Just $ Deck.InvestigatorDeck iid) (PlayerCard card)
                         , ResolvedCard iid (PlayerCard card)
                         ]
                  PlayerEnemyType -> do
                    guard $ not shuffleBackInEachWeakness
                    [DrewPlayerEnemy iid (PlayerCard card), ResolvedCard iid (PlayerCard card)]
                  other | hasRevelation card && other `notElem` [PlayerTreacheryType, PlayerEnemyType] -> do
                    guard $ ShuffleBackInEachWeakness `notElem` cardDrawRules cardDraw
                    [Revelation iid (PlayerCardSource card), ResolvedCard iid (PlayerCard card)]
                  _ -> []

            let
              weaknesses = map PlayerCard $ filter (`cardMatch` WeaknessCard) allDrawn
              msgs' =
                (<> msgs)
                  $ guard (shuffleBackInEachWeakness && notNull weaknesses)
                  *> [ FocusCards weaknesses
                     , chooseOne iid
                        $ [ Label
                              "Shuffle Weaknesses back in"
                              [ UnfocusCards
                              , ShuffleCardsIntoDeck
                                  (Deck.InvestigatorDeck iid)
                                  weaknesses
                              ]
                          ]
                     ]

            windowMsgs <-
              if null deck'
                then pure <$> checkWindows ((`mkWindow` Window.DeckHasNoCards iid) <$> [#when, #after])
                else pure []
            (before, _, after) <- frame $ Window.DrawCards iid $ map toCard allDrawn
            pushAll
              $ windowMsgs
              <> [DeckHasNoCards iid Nothing | null deck']
              <> [InvestigatorDrewPlayerCard iid card | card <- allDrawn]
              <> [before]
              <> msgs'
              <> [after]
            pure $ a & handL %~ (<> map PlayerCard allDrawn) & deckL .~ Deck deck'
  InvestigatorDrewPlayerCard iid card -> do
    windowMsg <-
      checkWindows [mkAfter (Window.DrawCard iid (PlayerCard card) $ Deck.InvestigatorDeck iid)]
    push windowMsg
    pure a
  InvestigatorSpendClues iid n | iid == investigatorId -> do
    pure $ a & tokensL %~ subtractTokens Clue n
  SpendResources iid _ | iid == investigatorId -> do
    push $ Do msg
    pure a
  Do (SpendResources iid n) | iid == investigatorId -> do
    pure $ a & tokensL %~ subtractTokens Resource n
  LoseResources iid source n | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen (Window.LostResources iid source n)]
    afterWindowMsg <- checkWindows [mkAfter (Window.LostResources iid source n)]
    pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
    pure a
  Do (LoseResources iid _ n) | iid == investigatorId -> do
    pure $ a & tokensL %~ subtractTokens Resource n
  LoseAllResources iid | iid == investigatorId -> pure $ a & tokensL %~ removeAllTokens Resource
  TakeResources iid n source True | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen (Window.PerformAction iid #resource)]
    afterWindowMsg <- checkWindows [mkAfter (Window.PerformAction iid #resource)]
    unlessM (hasModifier a CannotGainResources)
      $ pushAll
        [ BeginAction
        , beforeWindowMsg
        , TakeAction iid (Just #resource) (ActionCost 1)
        , CheckAttackOfOpportunity iid False
        , TakeResources iid n source False
        , afterWindowMsg
        , FinishAction
        ]
    pure a
  TakeResources iid n _ False | iid == investigatorId -> do
    cannotGainResources <- hasModifier a CannotGainResources
    pure $ if cannotGainResources then a else a & tokensL %~ addTokens Resource n
  PlaceTokens _ (isTarget a -> True) token n -> do
    pure $ a & tokensL %~ addTokens token n
  Do (EmptyDeck iid mDrawing) | iid == investigatorId -> do
    pushAll
      $ [EmptyDeck iid mDrawing]
      <> maybeToList mDrawing
      <> [Msg.InvestigatorDamage iid EmptyDeckSource 0 1]
    pure a
  EmptyDeck iid _ | iid == investigatorId -> do
    modifiers' <- getModifiers (toTarget a)
    pushWhen (CardsCannotLeaveYourDiscardPile `notElem` modifiers')
      $ ShuffleDiscardBackIn iid
    pure a
  AllDrawCardAndResource | not (a ^. defeatedL || a ^. resignedL) -> do
    unlessM (hasModifier a CannotDrawCards) $ do
      pushM $ drawCards investigatorId ScenarioSource 1
    mayChooseNotToTakeResources <-
      elem MayChooseNotToTakeUpkeepResources
        <$> getModifiers (InvestigatorTarget investigatorId)
    if mayChooseNotToTakeResources
      then do
        push
          $ chooseOne investigatorId
          $ [ Label "Do not take resource(s)" []
            , Label
                "Take resource(s)"
                [TakeResources investigatorId 1 (toSource a) False]
            ]
        pure a
      else pure $ a & tokensL %~ incrementTokens Resource
  LoadDeck iid deck | iid == investigatorId -> do
    shuffled <- shuffleM $ flip map (unDeck deck) $ \card ->
      card {pcOwner = Just iid}
    pure $ a & deckL .~ Deck shuffled
  InvestigatorCommittedCard iid card | iid == investigatorId -> do
    commitedCardWindows <- Helpers.windows [Window.CommittedCard iid card]
    pushAll $ FocusCards [card] : commitedCardWindows <> [UnfocusCards]
    pure
      $ a
      & (handL %~ filter (/= card))
      & (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
  PlaceUnderneath target cards | isTarget a target -> do
    update <-
      fmap appEndo
        $ foldMapM
          ( \card -> do
              mAssetId <- selectOne $ AssetWithCardId $ toCardId card
              pure
                $ Endo
                $ (assetsL %~ maybe id deleteSet mAssetId)
                . (slotsL %~ maybe id removeFromSlots mAssetId)
                . (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
          )
          cards
    pure $ a & cardsUnderneathL <>~ cards & update
  PlaceUnderneath _ cards -> do
    update <-
      fmap (appEndo . mconcat)
        $ traverse
          ( \card -> do
              mAssetId <- selectOne $ AssetWithCardId $ toCardId card
              pure
                $ Endo
                $ (assetsL %~ maybe id deleteSet mAssetId)
                . (slotsL %~ maybe id removeFromSlots mAssetId)
                . (deckL %~ Deck . filter ((/= card) . PlayerCard) . unDeck)
                . (handL %~ filter (/= card))
          )
          cards
    pure $ a & update & foundCardsL %~ Map.map (filter (`notElem` cards))
  ChaosTokenCanceled iid _ token | iid == investigatorId -> do
    whenWindow <- checkWindows [mkWhen (Window.CancelChaosToken iid token)]
    afterWindow <- checkWindows [mkAfter (Window.CancelChaosToken iid token)]
    pushAll [whenWindow, afterWindow]
    pure a
  ChaosTokenIgnored iid _ token | iid == toId a -> do
    whenWindow <- checkWindows [mkWhen (Window.IgnoreChaosToken iid token)]
    afterWindow <- checkWindows [mkAfter (Window.IgnoreChaosToken iid token)]
    pushAll [whenWindow, afterWindow]
    pure a
  BeforeSkillTest skillTest | skillTestInvestigator skillTest == toId a -> do
    skillTestModifiers' <- getModifiers SkillTestTarget
    push
      $ if RevealChaosTokensBeforeCommittingCards `elem` skillTestModifiers'
        then StartSkillTest investigatorId
        else CommitToSkillTest skillTest $ StartSkillTestButton investigatorId
    pure a
  CommitToSkillTest skillTest triggerMessage' | skillTestInvestigator skillTest == toId a -> do
    let iid = skillTestInvestigator skillTest
    modifiers' <- getModifiers (toTarget a)
    committedCards <- field InvestigatorCommittedCards iid
    allCommittedCards <- selectAgg id InvestigatorCommittedCards Anyone
    let
      skillDifficulty = skillTestDifficulty skillTest
      onlyCardComittedToTestCommitted =
        any
          (any (== OnlyCardCommittedToTest) . cdCommitRestrictions . toCardDef)
          allCommittedCards
      committedCardTitles = map toTitle allCommittedCards
    let window = mkWhen (Window.SkillTest $ skillTestType skillTest)
    actions <- getActions iid window
    isScenarioAbility <- getIsScenarioAbility
    clueCount <- field LocationClues investigatorLocation
    skillIcons <- getSkillTestMatchingSkillIcons

    skillTestModifiers' <- getModifiers SkillTestTarget
    cannotCommitCards <-
      elem (CannotCommitCards AnyCard) <$> getModifiers (InvestigatorTarget investigatorId)
    committableCards <-
      if cannotCommitCards || onlyCardComittedToTestCommitted
        then pure []
        else do
          committableTreacheries <-
            filterM (field TreacheryCanBeCommitted)
              =<< selectList (treacheryInHandOf investigatorId)
          treacheryCards <- traverse (field TreacheryCard) committableTreacheries
          flip filterM (investigatorHand <> treacheryCards) $ \case
            PlayerCard card -> do
              let
                passesCommitRestriction = \case
                  CommittableTreachery -> error "unhandled"
                  OnlyCardCommittedToTest -> pure $ null committedCardTitles
                  MaxOnePerTest -> pure $ toTitle card `notElem` committedCardTitles
                  OnlyYourTest -> pure True
                  MustBeCommittedToYourTest -> pure True
                  OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                  OnlyTestWithActions as ->
                    pure $ maybe False (`elem` as) (skillTestAction skillTest)
                  ScenarioAbility -> pure isScenarioAbility
                  SelfCanCommitWhen matcher -> notNull <$> select (You <> matcher)
                  MinSkillTestValueDifference n ->
                    case skillTestType skillTest of
                      SkillSkillTest skillType -> do
                        baseValue <- baseSkillValueFor skillType Nothing [] (toId a)
                        pure $ (skillDifficulty - baseValue) >= n
                      AndSkillTest types -> do
                        baseValue <-
                          sum
                            <$> traverse (\skillType -> baseSkillValueFor skillType Nothing [] (toId a)) types
                        pure $ (skillDifficulty - baseValue) >= n
                      ResourceSkillTest ->
                        pure $ (skillDifficulty - investigatorResources a) >= n
                prevented = flip any modifiers' $ \case
                  CanOnlyUseCardsInRole role ->
                    null
                      $ intersect
                        (cdClassSymbols $ toCardDef card)
                        (setFromList [Neutral, role])
                  CannotCommitCards matcher -> cardMatch card matcher
                  _ -> False
              passesCommitRestrictions <- allM passesCommitRestriction (cdCommitRestrictions $ toCardDef card)

              icons <- iconsForCard (toCard card)

              pure
                $ PlayerCard card
                `notElem` committedCards
                && (any (`member` skillIcons) icons || (null icons && toCardType card == SkillType))
                && passesCommitRestrictions
                && not prevented
            EncounterCard card ->
              pure $ CommittableTreachery `elem` (cdCommitRestrictions $ toCardDef card)
            VengeanceCard _ -> error "vengeance card"
    let
      mustCommit = any (any (== MustBeCommittedToYourTest) . cdCommitRestrictions . toCardDef) committableCards
      triggerMessage =
        [ triggerMessage'
        | CannotPerformSkillTest `notElem` skillTestModifiers' && not mustCommit
        ]
      beginMessage = CommitToSkillTest skillTest triggerMessage'
    if notNull committableCards || notNull committedCards || notNull actions
      then
        push
          $ SkillTestAsk
          $ chooseOne iid
          $ map
            (\card -> targetLabel (toCardId card) [SkillTestCommitCard iid card, beginMessage])
            committableCards
          <> map
            (\card -> targetLabel (toCardId card) [SkillTestUncommitCard iid card, beginMessage])
            committedCards
          <> map
            (\action -> AbilityLabel iid action [window] [beginMessage])
            actions
          <> triggerMessage
      else
        pushWhen (notNull triggerMessage)
          $ SkillTestAsk
          $ chooseOne iid triggerMessage
    pure a
  CommitToSkillTest skillTest triggerMessage | skillTestInvestigator skillTest /= investigatorId -> do
    let iid = skillTestInvestigator skillTest
    isPerlious <- getIsPerilous skillTest
    locationId <- getJustLocation iid
    isScenarioAbility <- getIsScenarioAbility
    clueCount <- field LocationClues locationId
    otherLocation <- field InvestigatorLocation (skillTestInvestigator skillTest)
    canCommit <- maybe (pure False) (canCommitToAnotherLocation a) otherLocation
    when (not isPerlious && (locationId == investigatorLocation || canCommit)) $ do
      committedCards <- field InvestigatorCommittedCards investigatorId
      allCommittedCards <- selectAgg id InvestigatorCommittedCards Anyone
      let
        skillDifficulty = skillTestDifficulty skillTest
        onlyCardComittedToTestCommitted =
          any
            ( any (== OnlyCardCommittedToTest) . cdCommitRestrictions . toCardDef
            )
            allCommittedCards
        committedCardTitles = map toTitle allCommittedCards
      modifiers' <- getModifiers (toTarget a)
      skillIcons <- getSkillTestMatchingSkillIcons
      let beginMessage = CommitToSkillTest skillTest triggerMessage
      committableCards <-
        if notNull committedCards || onlyCardComittedToTestCommitted
          then pure []
          else do
            committableTreacheries <-
              filterM (field TreacheryCanBeCommitted)
                =<< selectList (treacheryInHandOf investigatorId)
            treacheryCards <- traverse (field TreacheryCard) committableTreacheries
            flip filterM (investigatorHand <> treacheryCards) $ \case
              PlayerCard card -> do
                let
                  passesCommitRestriction = \case
                    CommittableTreachery -> error "unhandled"
                    MaxOnePerTest -> pure $ toTitle card `notElem` committedCardTitles
                    OnlyCardCommittedToTest -> pure $ null committedCardTitles
                    OnlyYourTest -> pure False
                    MustBeCommittedToYourTest -> pure False
                    OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                    OnlyTestWithActions as ->
                      pure $ maybe False (`elem` as) (skillTestAction skillTest)
                    ScenarioAbility -> pure isScenarioAbility
                    SelfCanCommitWhen matcher ->
                      notNull <$> select (You <> matcher)
                    MinSkillTestValueDifference n ->
                      case skillTestType skillTest of
                        SkillSkillTest skillType -> do
                          baseValue <- baseSkillValueFor skillType Nothing [] (toId a)
                          pure $ (skillDifficulty - baseValue) >= n
                        AndSkillTest types -> do
                          baseValue <-
                            sum
                              <$> traverse (\skillType -> baseSkillValueFor skillType Nothing [] (toId a)) types
                          pure $ (skillDifficulty - baseValue) >= n
                        ResourceSkillTest -> pure $ (skillDifficulty - investigatorResources a) >= n
                  prevented = flip any modifiers' $ \case
                    CanOnlyUseCardsInRole role ->
                      null $ intersect (cdClassSymbols $ toCardDef card) (setFromList [Neutral, role])
                    _ -> False
                passesCriterias <- allM passesCommitRestriction (cdCommitRestrictions $ toCardDef card)
                icons <- iconsForCard (toCard card)
                pure
                  $ and
                    [ PlayerCard card `notElem` committedCards
                    , or
                        [ any (`member` skillIcons) icons
                        , and [null icons, toCardType card == SkillType]
                        ]
                    , passesCriterias
                    , not prevented
                    ]
              EncounterCard card ->
                pure $ CommittableTreachery `elem` (cdCommitRestrictions $ toCardDef card)
              VengeanceCard _ -> error "vengeance card"
      pushWhen (notNull committableCards || notNull committedCards)
        $ SkillTestAsk
        $ chooseOne investigatorId
        $ map
          (\card -> targetLabel (toCardId card) [SkillTestCommitCard investigatorId card, beginMessage])
          committableCards
        <> map
          (\card -> targetLabel (toCardId card) [SkillTestUncommitCard investigatorId card, beginMessage])
          committedCards
    pure a
  CheckWindow iids windows | investigatorId `elem` iids -> do
    a <$ push (RunWindow investigatorId windows)
  RunWindow iid windows
    | iid == toId a
    , (not (investigatorDefeated || investigatorResigned) || Window.hasEliminatedWindow windows) -> do
        actions <- nub . concat <$> traverse (getActions iid) windows
        playableCards <- getPlayableCards a UnpaidCost windows
        unless (null playableCards && null actions) $ do
          anyForced <- anyM (isForcedAbility investigatorId) actions
          if anyForced
            then do
              let
                (silent, normal) = partition isSilentForcedAbility actions
                toForcedAbilities = map (($ windows) . UseAbility iid)
                toUseAbilities = map ((\f -> f windows []) . AbilityLabel iid)
              -- Silent forced abilities should trigger automatically
              pushAll
                $ toForcedAbilities silent
                <> [chooseOne iid (toUseAbilities normal) | notNull normal]
                <> [RunWindow iid windows]
            else do
              actionsWithMatchingWindows <-
                for actions $ \ability@Ability {..} ->
                  (ability,)
                    <$> filterM
                      ( \w -> windowMatches iid abilitySource w abilityWindow
                      )
                      windows
              skippable <- getAllAbilitiesSkippable a windows
              push
                $ chooseOne iid
                $ [ targetLabel (toCardId c)
                    $ [ InitiatePlayCard iid c Nothing windows False
                      , RunWindow iid windows
                      ]
                  | c <- playableCards
                  ]
                <> map
                  (\(ability, windows') -> AbilityLabel iid ability windows' [RunWindow iid windows])
                  actionsWithMatchingWindows
                <> [Label "Skip playing fast cards or using reactions" [] | skippable]
        pure a
  SpendActions iid source mAction n | iid == investigatorId -> do
    -- We want to try and spend the most restrictive action so we get any
    -- action that is not any additional action first, and if not that then the
    -- any additional action
    mAdditionalAction <-
      findM
        ( andM
            . sequence
              [ additionalActionCovers source mAction
              , pure . (/= AnyAdditionalAction)
              ]
        )
        investigatorAdditionalActions
    let mAnyAdditionalAction = find (== AnyAdditionalAction) investigatorAdditionalActions
    case mAdditionalAction <|> mAnyAdditionalAction of
      Nothing -> pure $ a & remainingActionsL %~ max 0 . subtract n
      Just aa -> pure $ a & additionalActionsL %~ deleteFirst aa
  UseEffectAction iid eid _ | iid == investigatorId -> do
    let
      isEffectAction = \case
        EffectAction _ eid' -> eid == eid'
        _ -> False
    pure $ a & additionalActionsL %~ filter (not . isEffectAction)
  LoseActions iid source n | iid == investigatorId -> do
    beforeWindowMsg <- checkWindows [mkWhen $ Window.LostActions iid source n]
    afterWindowMsg <- checkWindows [mkAfter $ Window.LostActions iid source n]
    pushAll [beforeWindowMsg, Do msg, afterWindowMsg]
    pure a
  Do (LoseActions iid source n) | iid == investigatorId -> do
    -- TODO: after losing all remaining actions we can lose additional actions
    let
      remaining = max 0 (n - a ^. remainingActionsL)
      additional = min remaining (length $ a ^. additionalActionsL)
      a' = a & remainingActionsL %~ max 0 . subtract n
    if additional > 0
      then
        if additional == length (a' ^. additionalActionsL)
          then pure $ a' & additionalActionsL .~ []
          else do
            push
              $ chooseN iid additional
              $ [ Label (additionalActionLabel ac) [LoseAdditionalAction iid source ac]
                | ac <- a' ^. additionalActionsL
                ]
            pure a'
      else pure a'
  SetActions iid _ n | iid == investigatorId -> do
    pure $ a & remainingActionsL .~ n
  GainActions iid _ n | iid == investigatorId -> do
    -- TODO: If we add a window here we need to reconsider Ace of Rods, likely it would need a Do variant
    pure $ a & remainingActionsL +~ n
  GainAdditionalAction iid _ n | iid == investigatorId -> do
    pure $ a & additionalActionsL %~ (n :)
  LoseAdditionalAction iid _ n | iid == investigatorId -> do
    pure $ a & additionalActionsL %~ deleteFirst n
  TakeAction iid mAction cost | iid == investigatorId -> do
    pushAll
      $ [PayForAbility (abilityEffect a cost) []]
      <> [TakenAction iid action | action <- maybeToList mAction]
    pure a
  TakenAction iid action | iid == investigatorId -> do
    pure $ a & actionsTakenL %~ (<> [action]) & actionsPerformedL %~ (<> [action])
  PerformedAction iid action | iid == investigatorId -> do
    pure $ a & actionsPerformedL %~ (<> [action])
  PutCardOnTopOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> do
    case card of
      PlayerCard pc ->
        pure
          $ a
          & (deckL %~ Deck . (pc :) . unDeck)
          & handL
          %~ filter (/= card)
          & discardL
          %~ filter (/= pc)
          & (foundCardsL . each %~ filter (/= PlayerCard pc))
      EncounterCard _ ->
        error "Can not put encounter card on top of investigator deck"
      VengeanceCard _ ->
        error "Can not put vengeance card on top of investigator deck"
  PutCardOnTopOfDeck _ _ card -> case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . filter (/= pc) . unDeck)
        & handL
        %~ filter (/= card)
        & discardL
        %~ filter (/= pc)
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ -> pure $ a & handL %~ filter (/= card)
    VengeanceCard vcard -> pure $ a & handL %~ filter (/= vcard)
  PutCardOnBottomOfDeck _ (Deck.InvestigatorDeck iid) card | iid == toId a -> do
    case card of
      PlayerCard pc ->
        pure
          $ a
          & (deckL %~ Deck . (<> [pc]) . unDeck)
          & handL
          %~ filter (/= card)
          & discardL
          %~ filter (/= pc)
          & (foundCardsL . each %~ filter (/= PlayerCard pc))
      EncounterCard _ ->
        error "Can not put encounter card on bottom of investigator deck"
      VengeanceCard _ ->
        error "Can not put vengeance card on bottom of investigator deck"
  PutCardOnBottomOfDeck _ _ card -> case card of
    PlayerCard pc ->
      pure
        $ a
        & (deckL %~ Deck . filter (/= pc) . unDeck)
        & handL
        %~ filter (/= card)
        & discardL
        %~ filter (/= pc)
        & (foundCardsL . each %~ filter (/= PlayerCard pc))
    EncounterCard _ -> pure a
    VengeanceCard _ -> pure a
  AddToHand iid cards | iid == investigatorId -> do
    let
      choices = mapMaybe cardChoice cards
      cardChoice = \case
        card@(PlayerCard card') -> do
          if hasRevelation card'
            then
              if toCardType card' == PlayerTreacheryType
                then Just (card, DrewTreachery iid Nothing card)
                else Just (card, Revelation iid $ PlayerCardSource card')
            else
              if toCardType card' == PlayerEnemyType
                then Just (card, DrewPlayerEnemy iid card)
                else Nothing
        _ -> Nothing
    pushWhen (notNull choices)
      $ chooseOrRunOneAtATime iid [targetLabel (toCardId card) [msg'] | (card, msg') <- choices]
    assetIds <- catMaybes <$> for cards (selectOne . AssetWithCardId . toCardId)
    pure
      $ a
      & handL
      %~ (cards <>)
      & cardsUnderneathL
      %~ filter (`notElem` cards)
      & assetsL
      %~ Set.filter (`notElem` assetIds)
      & slotsL
      %~ flip (foldr removeFromSlots) assetIds
      & discardL
      %~ filter ((`notElem` cards) . PlayerCard)
      & (foundCardsL . each %~ filter (`notElem` cards))
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [] | iid == investigatorId -> do
    -- can't shuffle zero cards
    pure a
  ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards | iid == toId a -> do
    let cards' = mapMaybe (preview _PlayerCard) cards
    deck <- shuffleM $ cards' <> filter (`notElem` cards') (unDeck investigatorDeck)
    pure
      $ a
      & deckL
      .~ Deck deck
      & handL
      %~ filter (`notElem` cards)
      & cardsUnderneathL
      %~ filter (`notElem` cards)
      & discardL
      %~ filter ((`notElem` cards) . PlayerCard)
      & (foundCardsL . each %~ filter (`notElem` cards))
  AddFocusedToHand _ (InvestigatorTarget iid') cardSource cardId | iid' == toId a -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (findWithDefault [] cardSource investigatorFoundCards)
      foundCards = investigatorFoundCards & ix cardSource %~ filter (/= card)
    push $ addToHand iid' card
    pure $ a & foundCardsL .~ foundCards
  AddFocusedToTopOfDeck _ (InvestigatorTarget iid') cardId | iid' == toId a -> do
    let
      card =
        fromJustNote "missing card"
          $ find ((== cardId) . toCardId) (concat $ toList investigatorFoundCards)
          >>= toPlayerCard
      foundCards = Map.map (filter ((/= cardId) . toCardId)) investigatorFoundCards
    push $ PutCardOnTopOfDeck iid' (Deck.InvestigatorDeck iid') (toCard card)
    pure $ a & foundCardsL .~ foundCards
  ShuffleAllFocusedIntoDeck _ (InvestigatorTarget iid') | iid' == investigatorId -> do
    let cards = findWithDefault [] Zone.FromDeck investigatorFoundCards
    push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid') cards
    pure $ a & foundCardsL %~ deleteMap Zone.FromDeck
  PutAllFocusedIntoDiscard _ (InvestigatorTarget iid') | iid' == investigatorId -> do
    let cards = onlyPlayerCards $ findWithDefault [] Zone.FromDiscard investigatorFoundCards
    pure $ a & foundCardsL %~ deleteMap Zone.FromDiscard & discardL <>~ cards
  DisengageEnemy iid eid | iid == investigatorId -> do
    canDisengage <- iid <=~> InvestigatorCanDisengage
    pure $ if canDisengage then a & engagedEnemiesL %~ deleteSet eid else a
  DisengageEnemyFromAll eid -> do
    canDisengage <- investigatorId <=~> InvestigatorCanDisengage
    pure $ if canDisengage then a & engagedEnemiesL %~ deleteSet eid else a
  EndSearch iid _ (InvestigatorTarget iid') cardSources | iid == investigatorId -> do
    push (SearchEnded iid)
    let
      foundKey = \case
        Zone.FromTopOfDeck _ -> Zone.FromDeck
        Zone.FromBottomOfDeck _ -> Zone.FromDeck
        other -> other
    for_ cardSources $ \(cardSource, returnStrategy) -> case returnStrategy of
      DiscardRest -> do
        push
          $ chooseOneAtATime iid
          $ map
            ( \case
                PlayerCard c -> targetLabel (toCardId c) [AddToDiscard iid c]
                EncounterCard c -> targetLabel (toCardId c) [AddToEncounterDiscard c]
                VengeanceCard _ -> error "not possible"
            )
            (findWithDefault [] Zone.FromDeck investigatorFoundCards)
      PutBackInAnyOrder -> do
        when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck")
        push
          $ chooseOneAtATime iid
          $ mapTargetLabelWith
            toCardId
            (\c -> [AddFocusedToTopOfDeck iid (toTarget iid') (toCardId c)])
            (findWithDefault [] Zone.FromDeck investigatorFoundCards)
      ShuffleBackIn -> do
        when (foundKey cardSource /= Zone.FromDeck) (error "Expects a deck")
        pushWhen (notNull investigatorFoundCards)
          $ ShuffleCardsIntoDeck
            (Deck.InvestigatorDeck iid)
            (findWithDefault [] Zone.FromDeck investigatorFoundCards)
      PutBack -> when (foundKey cardSource == Zone.FromDeck) (error "Can not take deck")
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            case abilityLimitType (abilityLimit usedAbility) of
              Just (PerSearch _) -> False
              _ -> True
        )
  EndSearch iid _ _ _ | iid == investigatorId -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            case abilityLimitType (abilityLimit usedAbility) of
              Just (PerSearch _) -> False
              _ -> True
        )
  SearchEnded iid | iid == investigatorId -> pure $ a & foundCardsL .~ mempty
  Search iid source target@(InvestigatorTarget iid') cardSources cardMatcher foundStrategy | iid' == toId a -> do
    let
      foundCards :: Map Zone [Card] =
        foldl'
          ( \hmap (cardSource, _) -> case cardSource of
              Zone.FromDeck ->
                insertWith (<>) Zone.FromDeck (map PlayerCard $ unDeck investigatorDeck) hmap
              Zone.FromTopOfDeck n ->
                insertWith (<>) Zone.FromDeck (map PlayerCard . take n $ unDeck investigatorDeck) hmap
              Zone.FromBottomOfDeck n ->
                insertWith (<>) Zone.FromDeck (map PlayerCard . take n . reverse $ unDeck investigatorDeck) hmap
              Zone.FromDiscard ->
                insertWith (<>) Zone.FromDiscard (map PlayerCard investigatorDiscard) hmap
              other -> error $ mconcat ["Zone ", show other, " not yet handled"]
          )
          mempty
          cardSources
      deck =
        filter
          ((`notElem` findWithDefault [] Zone.FromDeck foundCards) . PlayerCard)
          (unDeck investigatorDeck)
      targetCards = Map.map (filter (`cardMatch` cardMatcher)) foundCards
    push $ EndSearch iid source target cardSources
    case foundStrategy of
      RemoveFoundFromGame _ n -> do
        let
          choices =
            [ targetLabel (toCardId card) [RemovePlayerCardFromGame False card]
            | (_, cards) <- mapToList targetCards
            , card <- cards
            ]
        push
          $ if null choices
            then chooseOne iid [Label "No cards found" []]
            else chooseN iid (min n (length choices)) choices
      DrawFound who n -> do
        let
          choices =
            [ targetLabel (toCardId card) [AddFocusedToHand iid (toTarget who) zone (toCardId card)]
            | (zone, cards) <- mapToList targetCards
            , card <- cards
            ]
        push
          $ if null choices
            then chooseOne iid [Label "No cards found" []]
            else chooseN iid (min n (length choices)) choices
      DrawFoundUpTo who n -> do
        let
          choices =
            [ targetLabel (toCardId card) [AddFocusedToHand iid (toTarget who) zone (toCardId card)]
            | (zone, cards) <- mapToList targetCards
            , card <- cards
            ]
        push
          $ if null choices
            then chooseOne iid [Label "No cards found" []]
            else chooseUpToN iid n "Do not draw more cards" choices
      PlayFound who n -> do
        let windows' = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
        playableCards <- for (mapToList targetCards) $ \(zone, cards) -> do
          cards' <- filterM (getIsPlayable who source UnpaidCost windows') cards
          pure (zone, cards')
        let
          choices =
            [ targetLabel (toCardId card) [addToHand who card, PayCardCost iid card windows']
            | (_, cards) <- playableCards
            , card <- cards
            ]
        push $ chooseN iid n $ if null choices then [Label "No cards found" []] else choices
      PlayFoundNoCost who n -> do
        let windows' = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
        playableCards <- for (mapToList targetCards) $ \(zone, cards) -> do
          cards' <- filterM (getIsPlayable who source Cost.PaidCost windows') cards
          pure (zone, cards')
        let
          choices =
            [ targetLabel (toCardId card)
              $ [addToHand who card, PutCardIntoPlay iid card Nothing windows']
            | (_, cards) <- playableCards
            , card <- cards
            ]
        push $ chooseN iid n $ if null choices then [Label "No cards found" []] else choices
      DeferSearchedToTarget searchTarget -> do
        push
          $ if null targetCards
            then chooseOne iid [Label "No cards found" [SearchNoneFound iid searchTarget]]
            else SearchFound iid searchTarget (Deck.InvestigatorDeck iid') (concat $ toList targetCards)
      ReturnCards -> pure ()

    push $ CheckWindow [iid] [mkWhen (Window.AmongSearchedCards iid)]
    pure $ a & (deckL .~ Deck deck) & (foundCardsL .~ foundCards)
  RemoveFromDiscard iid cardId | iid == investigatorId -> do
    pure $ a & discardL %~ filter ((/= cardId) . toCardId)
  SufferTrauma iid physical mental | iid == investigatorId -> do
    push $ CheckTrauma iid
    pure $ a & physicalTraumaL +~ physical & mentalTraumaL +~ mental
  CheckTrauma iid | iid == investigatorId -> do
    pushWhen (investigatorPhysicalTrauma >= investigatorHealth)
      $ InvestigatorKilled (toSource a) iid
    pushWhen (investigatorMentalTrauma >= investigatorSanity)
      $ DrivenInsane iid
    pure a
  HealTrauma iid physical mental | iid == investigatorId -> do
    pure
      $ a
      & (physicalTraumaL %~ max 0 . subtract physical)
      & (mentalTraumaL %~ max 0 . subtract mental)
  GainXP iid _ amount | iid == investigatorId -> pure $ a & xpL +~ amount
  SpendXP iid amount | iid == investigatorId -> do
    pure $ a & xpL %~ max 0 . subtract amount
  InvestigatorPlaceCluesOnLocation iid source n | iid == investigatorId -> do
    let cluesToPlace = min n (investigatorClues a)
    push $ PlaceTokens source (LocationTarget investigatorLocation) Clue cluesToPlace
    pure $ a & tokensL %~ subtractTokens Clue cluesToPlace
  InvestigatorPlaceAllCluesOnLocation iid source | iid == investigatorId -> do
    push $ PlaceTokens source (LocationTarget investigatorLocation) Clue (investigatorClues a)
    pure $ a & tokensL %~ removeAllTokens Clue
  RemoveFromBearersDeckOrDiscard card -> do
    if pcOwner card == Just investigatorId
      then pure $ a & (discardL %~ filter (/= card)) & (deckL %~ Deck . filter (/= card) . unDeck)
      else pure a
  RemovePlayerCardFromGame addToRemovedFromGame card -> do
    when addToRemovedFromGame $ push $ RemovedFromGame card
    case preview _PlayerCard card of
      Just pc ->
        pure
          $ a
          & discardL
          %~ filter (/= pc)
          & handL
          %~ filter (/= card)
          & (deckL %~ Deck . filter (/= pc) . unDeck)
          & foundCardsL
          . each
          %~ filter (/= card)
      Nothing ->
        -- encounter cards can only be in hand
        pure $ a & (handL %~ filter (/= card))
  RemoveDiscardFromGame iid | iid == investigatorId -> do
    pushAll $ map (RemovedFromGame . PlayerCard) investigatorDiscard
    pure $ a & discardL .~ []
  After (FailedSkillTest iid mAction _ (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      windows = do
        Action.Investigate <- maybeToList mAction
        case mTarget of
          Just (LocationTarget lid) ->
            [ mkWhen $ Window.FailInvestigationSkillTest iid lid n
            , mkWhen $ Window.FailInvestigationSkillTest iid lid n
            ]
          _ ->
            [ mkWhen $ Window.FailInvestigationSkillTest iid investigatorLocation n
            , mkAfter $ Window.FailInvestigationSkillTest iid investigatorLocation n
            ]
    pushM
      $ checkWindows
      $ mkWhen (Window.FailSkillTest iid n)
      : mkAfter (Window.FailSkillTest iid n)
      : windows
    pure a
  When (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      windows = do
        Action.Investigate <- maybeToList mAction
        case mTarget of
          Just (ProxyTarget (LocationTarget lid) _) -> do
            [mkWhen $ Window.PassInvestigationSkillTest iid lid n]
          Just (LocationTarget lid) -> do
            [mkWhen $ Window.PassInvestigationSkillTest iid lid n]
          Just (BothTarget (LocationTarget lid1) (LocationTarget lid2)) ->
            [ mkWhen $ Window.PassInvestigationSkillTest iid lid1 n
            , mkWhen $ Window.PassInvestigationSkillTest iid lid2 n
            ]
          _ -> error "expecting location source for investigate"
    pushM $ checkWindows $ mkWhen (Window.PassSkillTest mAction source iid n) : windows
    pure a
  After (PassedSkillTest iid mAction source (InvestigatorTarget iid') _ n) | iid == iid' && iid == toId a -> do
    mTarget <- getSkillTestTarget
    let
      windows = do
        Action.Investigate <- maybeToList mAction
        case mTarget of
          Just (ProxyTarget (LocationTarget lid) _) -> do
            [mkAfter $ Window.PassInvestigationSkillTest iid lid n]
          Just (LocationTarget lid) -> do
            [mkAfter $ Window.PassInvestigationSkillTest iid lid n]
          Just (BothTarget (LocationTarget lid1) (LocationTarget lid2)) ->
            [ mkAfter $ Window.PassInvestigationSkillTest iid lid1 n
            , mkAfter $ Window.PassInvestigationSkillTest iid lid2 n
            ]
          _ -> error "expecting location source for investigate"
    pushM $ checkWindows $ mkAfter (Window.PassSkillTest mAction source iid n) : windows
    pure a
  PlayerWindow iid additionalActions isAdditional | iid == investigatorId -> do
    let
      windows = [mkWhen (Window.DuringTurn iid), mkWhen Window.FastPlayerWindow, mkWhen Window.NonFast]
    actions <- nub <$> concatMapM (getActions iid) windows
    anyForced <- anyM (isForcedAbility iid) actions
    if anyForced
      then do
        -- Silent forced abilities should trigger automatically
        let
          (silent, normal) = partition isSilentForcedAbility actions
          toForcedAbilities = map (($ windows) . UseAbility iid)
          toUseAbilities = map ((\f -> f windows []) . AbilityLabel iid)
        pushAll
          $ toForcedAbilities silent
          <> [chooseOne iid (toUseAbilities normal) | notNull normal]
          <> [PlayerWindow iid additionalActions isAdditional]
      else do
        modifiers <- getModifiers (InvestigatorTarget iid)
        canAffordTakeResources <- getCanAfford a [#resource]
        canAffordDrawCards <- getCanAfford a [#draw]
        let
          usesAction = not isAdditional
          drawCardsF = if usesAction then drawCardsAction else drawCards
          effectActions = flip mapMaybe investigatorAdditionalActions $ \case
            EffectAction tooltip effectId ->
              Just
                $ EffectActionButton
                  (Tooltip tooltip)
                  effectId
                  [UseEffectAction iid effectId windows]
            _ -> Nothing

        playableCards <- getPlayableCards a UnpaidCost windows
        drawing <- drawCardsF iid a 1

        let
          canDo action = not <$> anyM (prevents action) modifiers
          prevents action = \case
            CannotTakeAction x -> preventsAction action x
            MustTakeAction x -> not <$> preventsAction action x -- reads a little weird but we want only thing things x would prevent with cannot take action
            _ -> pure False
          preventsAction action = \case
            FirstOneOfPerformed as | action `elem` as -> do
              fieldP InvestigatorActionsPerformed (\taken -> all (`notElem` taken) as) iid
            FirstOneOfPerformed {} -> pure False
            IsAction action' -> pure $ action == action'
            EnemyAction {} -> pure False

        canDraw <- canDo #draw
        canTakeResource <- canDo #resource
        canPlay <- canDo #play

        push
          $ AskPlayer
          $ chooseOne iid
          $ additionalActions
          <> [ ComponentLabel (InvestigatorComponent iid ResourceToken)
              $ [TakeResources iid 1 (toSource a) usesAction]
             | canAffordTakeResources && CannotGainResources `notElem` modifiers && canTakeResource
             ]
          <> [ ComponentLabel (InvestigatorDeckComponent iid) [drawing]
             | canAffordDrawCards
             , canDraw
             , none (`elem` modifiers) [CannotDrawCards, CannotManipulateDeck]
             ]
          <> [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing windows usesAction]
             | canPlay
             , c <- playableCards
             ]
          <> [EndTurnButton iid [ChooseEndTurn iid]]
          <> map ((\f -> f windows []) . AbilityLabel iid) actions
          <> effectActions
    pure a
  PlayerWindow iid additionalActions isAdditional | iid /= investigatorId -> do
    let windows = [mkWhen (Window.DuringTurn iid), mkWhen Window.FastPlayerWindow]
    actions <- nub <$> concatMapM (getActions investigatorId) windows
    anyForced <- anyM (isForcedAbility investigatorId) actions
    unless anyForced $ do
      playableCards <- getPlayableCards a UnpaidCost windows
      let
        usesAction = not isAdditional
        choices =
          additionalActions
            <> [ targetLabel (toCardId c)
                $ [InitiatePlayCard investigatorId c Nothing windows usesAction]
               | c <- playableCards
               ]
            <> map ((\f -> f windows []) . AbilityLabel investigatorId) actions
      unless (null choices)
        $ push
        $ AskPlayer
        $ chooseOne investigatorId choices
    pure a
  EndInvestigation -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerPhase
        )
  EndEnemy -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerPhase
        )
  ScenarioCountIncrementBy CurrentDepth n | n > 0 -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerDepthLevel
        )
  EndUpkeep -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerPhase
        )
  EndMythos -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerPhase
        )
  EndRound -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerRound
        )
      & additionalActionsL
      .~ mempty
  EndTurn iid | iid == toId a -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter
        ( \UsedAbility {..} ->
            abilityLimitType (abilityLimit usedAbility) /= Just PerTurn
        )
  UseCardAbility iid (isSource a -> True) 500 _ _ -> do
    otherInvestigators <-
      selectList $ colocatedWith investigatorId <> NotInvestigator (InvestigatorWithId investigatorId)
    case nonEmpty otherInvestigators of
      Nothing -> error "No other investigators"
      Just (x :| xs) ->
        push
          $ chooseOrRunOne iid
          $ [ targetLabel iid'
              $ [ chooseSome1 iid "Done giving keys"
                    $ [Label ("Give " <> keyName k <> " key") [PlaceKey (toTarget iid') k] | k <- toList investigatorKeys]
                ]
            | iid' <- x : xs
            ]
    pure a
  UseAbility iid ability windows | iid == investigatorId -> do
    pushAll [PayForAbility ability windows, ResolvedAbility ability]
    case find ((== ability) . usedAbility) investigatorUsedAbilities of
      Nothing -> do
        depth <- getWindowDepth
        let
          used =
            UsedAbility
              { usedAbility = ability
              , usedAbilityInitiator = iid
              , usedAbilityWindows = windows
              , usedTimes = 1
              , usedDepth = depth
              }
        pure $ a & usedAbilitiesL %~ (used :)
      Just _ -> do
        let
          updateUsed used
            | usedAbility used == ability = used {usedTimes = usedTimes used + 1}
            | otherwise = used
        pure $ a & usedAbilitiesL %~ map updateUsed
  DoNotCountUseTowardsAbilityLimit iid ability | iid == investigatorId -> do
    let
      updateUsed used
        | usedAbility used == ability = used {usedTimes = max 0 (usedTimes used - 1)}
        | otherwise = used
    pure $ a & usedAbilitiesL %~ map updateUsed
  SkillTestEnds _ _ -> do
    pure
      $ a
      & usedAbilitiesL
      %~ filter (\UsedAbility {..} -> abilityLimitType (abilityLimit usedAbility) /= Just PerTestOrAbility)
  PickSupply iid s | iid == toId a -> pure $ a & suppliesL %~ (s :)
  UseSupply iid s | iid == toId a -> pure $ a & suppliesL %~ deleteFirst s
  Blanked msg' -> runMessage msg' a
  _ -> pure a

getFacingDefeat :: HasGame m => InvestigatorAttrs -> m Bool
getFacingDefeat a@InvestigatorAttrs {..} = do
  canOnlyBeDefeatedByDamage <- hasModifier a CanOnlyBeDefeatedByDamage
  modifiedHealth <- getModifiedHealth a
  modifiedSanity <- getModifiedSanity a
  pure
    $ or
      [ investigatorHealthDamage a + investigatorAssignedHealthDamage >= modifiedHealth
      , and
          [ investigatorSanityDamage a + investigatorAssignedSanityDamage >= modifiedSanity
          , not canOnlyBeDefeatedByDamage
          ]
      ]

getModifiedHealth :: HasGame m => InvestigatorAttrs -> m Int
getModifiedHealth attrs@InvestigatorAttrs {..} = do
  foldr applyModifier investigatorHealth <$> getModifiers attrs
 where
  applyModifier (HealthModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getModifiedSanity :: HasGame m => InvestigatorAttrs -> m Int
getModifiedSanity attrs@InvestigatorAttrs {..} = do
  foldr applyModifier investigatorSanity <$> getModifiers attrs
 where
  applyModifier (SanityModifier m) n = max 0 (n + m)
  applyModifier _ n = n
