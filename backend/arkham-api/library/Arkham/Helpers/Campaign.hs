module Arkham.Helpers.Campaign where

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace, isSymbolChaosToken)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map

completedScenario :: HasGame m => ScenarioId -> m Bool
completedScenario cCode = elem cCode <$> getCompletedScenarios

getCompletedScenarios :: HasGame m => m (Set ScenarioId)
getCompletedScenarios = setFromList <$> getCompletedScenariosList

getCompletedSteps :: HasGame m => m [CampaignStep]
getCompletedSteps =
  selectOne TheCampaign >>= \case
    Nothing -> pure mempty
    Just campaignId -> field CampaignCompletedSteps campaignId

getCompletedScenariosList :: HasGame m => m [ScenarioId]
getCompletedScenariosList = do
  selectOne TheCampaign >>= \case
    Nothing -> pure mempty
    Just campaignId -> do
      completedSteps <- field CampaignCompletedSteps campaignId
      pure
        $ flip mapMaybe completedSteps
        $ \case
          ScenarioStep scenarioId -> Just scenarioId
          _ -> Nothing

getOwner :: HasGame m => CardDef -> m (Maybe InvestigatorId)
getOwner cardDef = do
  iids <- select $ IncludeEliminated Anyone
  cardMap <- getCampaignStoryCards
  let inGame = Map.filterWithKey (\k _ -> k `elem` iids) cardMap
  pure $ findKey (any ((== cardDef) . toCardDef)) inGame

withOwner :: HasGame m => CardDef -> (InvestigatorId -> m ()) -> m ()
withOwner cardDef f =
  getOwner cardDef >>= \case
    Nothing -> pure ()
    Just iid -> f iid

getCampaignStoryCards :: HasGame m => m (Map InvestigatorId [Card])
getCampaignStoryCards = do
  mCampaignId <- selectOne TheCampaign
  case mCampaignId of
    Just campaignId -> field CampaignStoryCards campaignId
    Nothing -> scenarioField ScenarioStoryCards

getCampaignStoryCard :: (HasCallStack, HasGame m) => CardDef -> m Card
getCampaignStoryCard def = fromJustNote "missing card" <$> getMaybeCampaignStoryCard def

getMaybeCampaignStoryCard :: (HasGame m, HasCardCode def) => def -> m (Maybe Card)
getMaybeCampaignStoryCard (toCardCode -> cardCode) = do
  cards <- concat . Map.elems <$> getCampaignStoryCards
  pure $ find ((== toCardCode cardCode) . toCardCode) cards

getIsAlreadyOwned :: HasGame m => CardDef -> m Bool
getIsAlreadyOwned cDef = any (any ((== cDef) . toCardDef)) . toList <$> getCampaignStoryCards

campaignField :: (HasCallStack, HasGame m) => Field Campaign a -> m a
campaignField fld = selectJust TheCampaign >>= field fld

{- | "Draw tokens from the chaos bag at random until you have @n@ non-symbol
tokens. Replace each of these tokens with a chaos token of a lower value for the
remainder of the campaign. (If you are unable to replace a token, repeat this
process until a total of @n@ chaos tokens have been replaced.)"

@lower@ gives the replacement face, or Nothing when the token is already at the
floor and cannot be replaced; those are drawn, dropped from the pool, and the draw
repeats — which is what "repeat this process" means. Symbol tokens are never drawn
at all, so the pool only ever holds the non-symbol tokens still undrawn.

@render@ receives each (original, replacement) pair in draw order, and is skipped
entirely when nothing was replaced. Draw it with
'Arkham.Helpers.FlavorText.chaosTokenMorph' so the player sees which tokens came
out of the bag before they flip to their new faces. It is queued ahead of the bag
update deliberately: the morph plays against the old bag, and the chaos bag only
shows its new contents once the player dismisses the story.
-}
replaceCampaignChaosTokens
  :: ReverseQueue m
  => Int
  -> (ChaosTokenFace -> Maybe ChaosTokenFace)
  -> ([(ChaosTokenFace, ChaosTokenFace)] -> m ())
  -> m ()
replaceCampaignChaosTokens n lower render = do
  bag <- campaignField CampaignChaosBag
  (newBag, replaced) <- go n bag bag []
  unless (null replaced) $ render replaced
  push $ SetCampaignChaosBag newBag
 where
  go 0 bag _ acc = pure (bag, reverse acc)
  go k bag pool acc = case nonEmpty (filter (not . isSymbolChaosToken) pool) of
    Nothing -> pure (bag, reverse acc)
    Just nonSymbols -> do
      face <- sample nonSymbols
      let pool' = deleteFirstMatch (== face) pool
      case lower face of
        Just lowered -> go (k - 1) (replaceFirstMatch face lowered bag) pool' ((face, lowered) : acc)
        Nothing -> go k bag pool' acc
  replaceFirstMatch :: ChaosTokenFace -> ChaosTokenFace -> [ChaosTokenFace] -> [ChaosTokenFace]
  replaceFirstMatch _ _ [] = []
  replaceFirstMatch x x' (y : ys) = if x == y then x' : ys else y : replaceFirstMatch x x' ys

getCampaignMeta :: forall a m. (HasCallStack, HasGame m, FromJSON a) => m a
getCampaignMeta = do
  result <- fromJSON @a <$> campaignField CampaignMeta
  case result of
    Success a -> pure a
    Error e -> error $ "Failed to parse campaign meta: " <> e

{- | 'getCampaignMeta' for code that also runs in standalone mode, where there is
no @Campaign@ entity at all and 'campaignField' would throw. A malformed meta is
still an error; only a missing campaign is @Nothing@.
-}
getCampaignMetaMaybe :: forall a m. (HasCallStack, HasGame m, FromJSON a) => m (Maybe a)
getCampaignMetaMaybe =
  selectOne TheCampaign >>= \case
    Nothing -> pure Nothing
    Just campaignId -> do
      result <- fromJSON @a <$> field CampaignMeta campaignId
      case result of
        Success a -> pure (Just a)
        Error e -> error $ "Failed to parse campaign meta: " <> e

withCampaignMeta
  :: forall a m r. (HasCallStack, HasGame m, FromJSON a) => (a -> r) -> m r
withCampaignMeta f = f <$> getCampaignMeta @a

getCampaignStore :: (HasCallStack, HasGame m) => m (Map Text Value)
getCampaignStore = campaignField CampaignStore

stored :: forall a m. (HasCallStack, HasGame m, FromJSON a) => Text -> m (Maybe a)
stored k = do
  store <- getCampaignStore
  pure $ case lookup k store of
    Nothing -> Nothing
    Just v -> case fromJSON v of
      Success a -> Just a
      Error e -> error $ "Failed to parse stored value: " <> e

{- | The canonical card codes of the basic weaknesses @iid@ can no longer find when
told to "search the collection" for one.

Each investigator is treated as bringing their own collection, so a weakness already
taken only blocks the player who took it — two investigators may each end up with
their own copy of the same weakness. Unique weaknesses are the exception: only one
can exist at the table, so any investigator holding one rules it out for everyone.

Covers campaign story cards as well as saved decks, because
'AddCampaignCardToDeck' files permanently-added weaknesses (including each
investigator's starting random one) under story cards rather than the deck.

Keyed on 'canonicalCardCode', so holding any one printing of Stubborn Detective
rules out its other printings too — reprints are separate 'CardDef's (#5346).
-}
getTakenBasicWeaknesses :: HasGame m => InvestigatorId -> m (Set CardCode)
getTakenBasicWeaknesses iid = do
  decks <- withStandalone (field CampaignDecks) (field ScenarioPlayerDecks)
  storyCards <- getCampaignStoryCards
  let defsFor :: IsCard card => [card] -> [CardDef]
      defsFor cards = [toCardDef c | c <- cards, c `cardMatch` BasicWeaknessCard]
      held owner =
        maybe [] (defsFor . unDeck) (lookup owner decks)
          <> defsFor (findWithDefault [] owner storyCards)
      everyone = concatMap held (ordNub $ keys decks <> keys storyCards)
  pure
    $ setFromList
    $ map canonicalCardCode
    $ held iid
    <> filter cdUnique everyone

matchingCardsAlreadyInDeck
  :: HasGame m => CardMatcher -> m (Map InvestigatorId (Set CardCode))
matchingCardsAlreadyInDeck matcher = do
  decks <- withStandalone (field CampaignDecks) (field ScenarioPlayerDecks)
  pure $ Map.map (setFromList . map toCardCode . filter (`cardMatch` matcher) . unDeck) decks

addCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> Message
addCampaignCardToDeckChoice leadPlayer investigators shouldShuffleIn card =
  addCampaignCardToDeckChoiceWith leadPlayer investigators shouldShuffleIn card (const [])

addCampaignCardToDeckChoiceWith
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> (InvestigatorId -> [Message]) -> Message
addCampaignCardToDeckChoiceWith leadPlayer investigators shouldShuffleIn card f =
  addCampaignCardToDeckChoiceWhenDeclined leadPlayer investigators shouldShuffleIn card f []

{- | Like 'addCampaignCardToDeckChoiceWith', but with messages to run when the
players decline the card (e.g. the "I Don't Trust Her" achievement).
-}
addCampaignCardToDeckChoiceWhenDeclined
  :: PlayerId
  -> [InvestigatorId]
  -> ShuffleIn
  -> Card
  -> (InvestigatorId -> [Message])
  -> [Message]
  -> Message
addCampaignCardToDeckChoiceWhenDeclined leadPlayer investigators shouldShuffleIn card f declined = withI18n do
  questionLabelWithCard (cardNameVar card $ ikey' "label.addCardToDeck") card.cardCode leadPlayer
    $ ChooseOne
    $ [ PortraitLabel investigator $ AddCampaignCardToDeck investigator shouldShuffleIn card
          : f investigator
      | investigator <- investigators
      ]
    <> [Label (cardNameVar card $ ikey' "label.doNotAddCardToDeck") declined]

forceAddCampaignCardToDeckChoice
  :: PlayerId -> [InvestigatorId] -> ShuffleIn -> Card -> Message
forceAddCampaignCardToDeckChoice _ [onlyId] shouldShuffleIn card = AddCampaignCardToDeck onlyId shouldShuffleIn card
forceAddCampaignCardToDeckChoice leadPlayer investigators shouldShuffleIn card = withI18n do
  questionLabelWithCard (cardNameVar card $ ikey' "label.addCardToDeck") card.cardCode leadPlayer
    $ ChooseOne
      [ PortraitLabel investigator [AddCampaignCardToDeck investigator shouldShuffleIn card]
      | investigator <- investigators
      ]

getCurrentDeck
  :: (HasCallStack, HasGame m, ToId investigator InvestigatorId)
  => investigator -> m (Deck PlayerCard)
getCurrentDeck (asId -> iid) =
  field InvestigatorDeck iid >>= \case
    Deck [] -> do
      decks <- withStandalone (field CampaignDecks) (field ScenarioPlayerDecks)
      case Map.lookup iid decks of
        Nothing -> pure $ Deck []
        Just deck -> do
          allStoryCards <- withStandalone (field CampaignStoryCards) (field ScenarioStoryCards)
          let storyCards = findWithDefault [] iid allStoryCards
          let deck' = fst $ partitionReloadedDeck storyCards [] (unDeck deck)
          pure $ Deck $ deck' <> mapMaybe (preview _PlayerCard) storyCards
    deck -> pure deck
