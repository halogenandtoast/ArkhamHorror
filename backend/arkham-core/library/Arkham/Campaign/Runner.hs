{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaign.Runner (
  module X,
  defaultCampaignRunner,
) where

import Arkham.Prelude

import Arkham.Campaign.Types as X
import Arkham.Helpers.Message as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.PlayerCard (setPlayerCardOwner)
import Arkham.ChaosToken
import Arkham.Classes.Entity
import Arkham.Classes.GameLogger
import Arkham.Classes.RunMessage
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Name
import Data.Map.Strict qualified as Map

defaultCampaignRunner :: IsCampaign a => Runner a
defaultCampaignRunner msg a = case msg of
  StartCampaign -> do
    -- [ALERT] StartCampaign
    players <- allPlayers
    lead <- getActivePlayer
    pushAll
      $ chooseDecks players
      : [Ask lead PickCampaignSettings | campaignStep (toAttrs a) /= PrologueStep]
        <> [CampaignStep $ campaignStep $ toAttrs a]
    pure a
  CampaignStep (ScenarioStep sid) -> do
    pushAll [ResetInvestigators, ResetGame, StartScenario sid]
    -- [ALERT] Update TheDreamEaters if this alters a
    pure a
  CampaignStep (UpgradeDeckStep _) -> do
    players <- allPlayers
    pushAll $ ResetGame : map chooseUpgradeDeck players <> [FinishedUpgradingDecks]
    pure a
  SetChaosTokensForScenario -> a <$ push (SetChaosTokens $ campaignChaosBag $ toAttrs a)
  AddCampaignCardToDeck iid cardDef -> do
    card <- genPlayerCardWith cardDef (setPlayerCardOwner iid)
    pure $ updateAttrs a (storyCardsL %~ insertWith (<>) iid [card])
  RemoveCampaignCard cardDef -> do
    pure $ updateAttrs a $ \attrs ->
      attrs
        & storyCardsL
        %~ Map.map (filter ((/= cardDef) . toCardDef))
        & decksL
        %~ Map.map (withDeck (filter ((/= cardDef) . toCardDef)))
  RemoveCampaignCardFromDeck iid cardDef ->
    pure $ updateAttrs a $ \attrs ->
      attrs
        & storyCardsL
        %~ adjustMap (filter ((/= cardDef) . toCardDef)) iid
        & decksL
        %~ adjustMap (withDeck (filter ((/= cardDef) . toCardDef))) iid
  AddChaosToken token -> do
    if token `notElem` [CurseToken, BlessToken]
      then pure $ updateAttrs a (chaosBagL %~ (token :))
      else pure a
  RemoveAllChaosTokens token -> pure $ updateAttrs a (chaosBagL %~ filter (/= token))
  InitDeck iid deck -> do
    playerCount <- getPlayerCount
    (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded playerCount deck
    pid <- getPlayer iid
    purchaseTrauma <- initDeckTrauma deck' iid pid CampaignTarget
    pushAll
      $ map (AddCampaignCardToDeck iid) randomWeaknesses
      <> purchaseTrauma

    pure $ updateAttrs a $ decksL %~ insertMap iid deck'
  ResolveAmounts iid choiceMap (LabeledTarget "Purchase Trauma" CampaignTarget) -> do
    let physical = getChoiceAmount "Physical" choiceMap
    let mental = getChoiceAmount "Mental" choiceMap
    push $ SufferTrauma iid physical mental
    pure a
  UpgradeDeck iid deck -> do
    pid <- getPlayer iid
    let
      oldDeck = fromJustNote "No deck?" $ lookup iid (campaignDecks $ toAttrs a)
      deckDiff =
        foldr
          (\x -> deleteFirstMatch ((== toCardCode x) . toCardCode))
          (unDeck deck)
          (unDeck oldDeck)

    purchaseTrauma <- initDeckTrauma (Deck deckDiff) iid pid CampaignTarget
    -- We remove the random weakness if the upgrade deck still has it listed
    -- since this will have been added at the beginning of the campaign
    playerCount <- getPlayerCount
    (deck', _) <- addRandomBasicWeaknessIfNeeded playerCount deck
    pushAll purchaseTrauma
    pure $ updateAttrs a $ decksL %~ insertMap iid deck'
  FinishedUpgradingDecks -> case campaignStep (toAttrs a) of
    UpgradeDeckStep nextStep' -> do
      push $ CampaignStep nextStep'
      pure $ updateAttrs a $ stepL .~ nextStep'
    _ -> error "invalid state"
  ResetGame -> do
    for_ (mapToList $ campaignDecks $ toAttrs a) $ \(iid, deck) -> do
      let investigatorStoryCards = findWithDefault [] iid (campaignStoryCards $ toAttrs a)
      push (LoadDeck iid . Deck $ unDeck deck <> investigatorStoryCards)
    pure a
  CrossOutRecord key -> do
    let
      crossedOutModifier =
        if key `member` view (logL . recordedL) (toAttrs a) then insertSet key else id
      removeOrderedKey =
        if key `member` view (logL . recordedL) (toAttrs a) then filter (/= key) else id

    pure
      $ updateAttrs a
      $ (logL . recordedL %~ deleteSet key)
      . (logL . crossedOutL %~ crossedOutModifier)
      . (logL . recordedSetsL %~ deleteMap key)
      . (logL . recordedCountsL %~ deleteMap key)
      . (logL . orderedKeysL %~ removeOrderedKey)
  Record key -> do
    send $ "Record \"" <> format key <> "\""
    pure
      $ updateAttrs a
      $ ( logL
            . recordedL
            %~ insertSet key
        )
      . ( logL
            . orderedKeysL
            %~ (<> [key])
        )
  RecordSetInsert key recs -> do
    let defs = mapMaybe lookupCardDef $ recordedCardCodes recs
    for_ defs $ \def ->
      send $ "Record \"" <> format (toName def) <> " " <> format key <> "\""
    pure $ case (toAttrs a) ^. logL . recordedSetsL . at key of
      Nothing ->
        updateAttrs a $ logL . recordedSetsL %~ insertMap key recs
      Just set ->
        let
          set' =
            filter (`notElem` recs) set
              <> recs
         in
          updateAttrs a $ logL . recordedSetsL %~ insertMap key set'
  CrossOutRecordSetEntries key recs ->
    pure
      $ updateAttrs a
      $ ( logL
            . recordedSetsL
            %~ adjustMap
              ( map
                  ( \case
                      someRec@(SomeRecorded k (Recorded c))
                        | someRec `elem` recs ->
                            SomeRecorded k (CrossedOut c)
                      other -> other
                  )
              )
              key
        )
  RecordCount key int ->
    pure $ updateAttrs a $ logL . recordedCountsL %~ insertMap key int
  IncrementRecordCount key int ->
    pure $ updateAttrs a $ logL . recordedCountsL %~ alterMap (Just . maybe int (+ int)) key
  ScenarioResolution r -> case campaignStep (toAttrs a) of
    ScenarioStep sid -> pure $ updateAttrs a $ resolutionsL %~ insertMap sid r
    _ -> error "must be called in a scenario"
  DrivenInsane iid ->
    pure
      $ updateAttrs a
      $ logL
      . recordedSetsL
      %~ insertWith
        (<>)
        DrivenInsaneInvestigators
        (singleton $ recorded $ unInvestigatorId iid)
  InvestigatorKilled _ iid ->
    pure
      $ updateAttrs a
      $ logL
      . recordedSetsL
      %~ insertWith
        (<>)
        KilledInvestigators
        (singleton $ recorded $ unInvestigatorId iid)
  CreateWeaknessInThreatArea (PlayerCard pc) iid -> do
    pure
      $ updateAttrs a
      $ decksL
      %~ adjustMap (withDeck (pc {pcOwner = Just iid} :)) iid
  AddCardToDeckForCampaign iid pc -> do
    pure
      $ updateAttrs a
      $ decksL
      %~ adjustMap (withDeck (pc {pcOwner = Just iid} :)) iid
  RemoveCardFromDeckForCampaign iid pc ->
    pure $ updateAttrs a $ decksL %~ adjustMap (withDeck (filter (/= pc))) iid
  NextCampaignStep mOverrideStep -> do
    let mstep = mOverrideStep <|> nextStep a
    push $ maybe GameOver CampaignStep mstep
    pure
      $ updateAttrs a
      $ \attrs ->
        attrs & (stepL %~ maybe id const mstep) & (completedStepsL %~ completeStep (campaignStep attrs))
  SetCampaignLog newLog -> do
    pushAll $ map HandleOption (toList $ campaignLogOptions newLog)
    pure $ updateAttrs a $ logL .~ newLog
  _ -> pure a
