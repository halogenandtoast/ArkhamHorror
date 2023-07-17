{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaign.Runner (
  module X,
) where

import Arkham.Prelude

import Arkham.Campaign.Types as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message
import Arkham.Name
import Data.Map.Strict qualified as Map

instance RunMessage CampaignAttrs where
  runMessage msg a@CampaignAttrs {..} = case msg of
    StartCampaign -> do
      pushAll $ map HandleOption (toList $ campaignLogOptions campaignLog) <> [CampaignStep campaignStep]
      pure a
    CampaignStep Nothing -> a <$ push GameOver -- TODO: move to generic
    CampaignStep (Just (ScenarioStep sid)) -> do
      a <$ pushAll [ResetInvestigators, ResetGame, StartScenario sid]
    CampaignStep (Just (UpgradeDeckStep _)) -> do
      investigatorIds <- allInvestigatorIds
      a
        <$ pushAll
          ( ResetGame
              : map chooseUpgradeDeck investigatorIds
                <> [FinishedUpgradingDecks]
          )
    SetChaosTokensForScenario -> a <$ push (SetChaosTokens campaignChaosBag)
    AddCampaignCardToDeck iid cardDef -> do
      card <- genPlayerCard cardDef
      pure $
        a
          & storyCardsL
            %~ insertWith
              (<>)
              iid
              [card {pcOwner = Just iid}]
    RemoveCampaignCard cardDef -> do
      pure $
        a
          & storyCardsL
            %~ Map.map (filter ((/= cardDef) . toCardDef))
          & decksL
            %~ Map.map (withDeck (filter ((/= cardDef) . toCardDef)))
    RemoveCampaignCardFromDeck iid cardDef ->
      pure $
        a
          & storyCardsL
            %~ adjustMap (filter ((/= cardDef) . toCardDef)) iid
          & decksL
            %~ adjustMap (withDeck (filter ((/= cardDef) . toCardDef))) iid
    AddChaosToken token -> pure $ a & chaosBagL %~ (token :)
    RemoveAllChaosTokens token -> pure $ a & chaosBagL %~ filter (/= token)
    InitDeck iid deck -> do
      (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
      let
        mentalTrauma =
          getSum $
            foldMap
              (Sum . fromMaybe 0 . cdPurchaseMentalTrauma . toCardDef)
              deck'
      pushAll $
        map (AddCampaignCardToDeck iid) randomWeaknesses
          <> [SufferTrauma iid 0 mentalTrauma | mentalTrauma > 0]

      pure $ a & decksL %~ insertMap iid deck'
    UpgradeDeck iid deck -> do
      let
        oldDeck = fromJustNote "No deck?" $ lookup iid campaignDecks
        deckDiff =
          foldr
            (\x -> deleteFirstMatch ((== toCardCode x) . toCardCode))
            (unDeck deck)
            (unDeck oldDeck)
        mentalTrauma =
          getSum $
            foldMap
              (Sum . fromMaybe 0 . cdPurchaseMentalTrauma . toCardDef)
              deckDiff
      -- We remove the random weakness if the upgrade deck still has it listed
      -- since this will have been added at the beginning of the campaign
      (deck', _) <- addRandomBasicWeaknessIfNeeded deck
      when (mentalTrauma > 0) $ push $ SufferTrauma iid 0 mentalTrauma
      pure $ a & decksL %~ insertMap iid deck'
    FinishedUpgradingDecks -> case a ^. stepL of
      Just (UpgradeDeckStep nextStep) -> do
        push (CampaignStep $ Just nextStep)
        pure $ a & stepL ?~ nextStep
      _ -> error "invalid state"
    ResetGame -> do
      for_ (mapToList campaignDecks) $ \(iid, deck) -> do
        let investigatorStoryCards = findWithDefault [] iid campaignStoryCards
        push (LoadDeck iid . Deck $ unDeck deck <> investigatorStoryCards)
      pure a
    CrossOutRecord key -> do
      let
        crossedOutModifier =
          if key `member` view (logL . recordedL) a then insertSet key else id
        removeOrderedKey =
          if key `member` view (logL . recordedL) a then filter (/= key) else id

      pure $
        a
          & (logL . recordedL %~ deleteSet key)
          & (logL . crossedOutL %~ crossedOutModifier)
          & (logL . recordedSetsL %~ deleteMap key)
          & (logL . recordedCountsL %~ deleteMap key)
          & (logL . orderedKeysL %~ removeOrderedKey)
    Record key -> do
      send $ "Record \"" <> format key <> "\""
      pure $
        a
          & logL
            . recordedL
            %~ insertSet key
          & logL
            . orderedKeysL
            %~ (<> [key])
    RecordSetInsert key recs -> do
      let defs = mapMaybe lookupCardDef $ recordedCardCodes recs
      for_ defs $ \def ->
        send $ "Record \"" <> format (toName def) <> " " <> format key <> "\""
      pure $ case a ^. logL . recordedSetsL . at key of
        Nothing ->
          a & logL . recordedSetsL %~ insertMap key recs
        Just set ->
          let
            set' =
              filter (`notElem` recs) set
                <> recs
          in
            a & logL . recordedSetsL %~ insertMap key set'
    CrossOutRecordSetEntries key recs ->
      pure $
        a
          & ( logL
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
      pure $ a & logL . recordedCountsL %~ insertMap key int
    ScenarioResolution r -> case campaignStep of
      Just (ScenarioStep sid) -> pure $ a & resolutionsL %~ insertMap sid r
      _ -> error "must be called in a scenario"
    DrivenInsane iid ->
      pure $
        a
          & logL . recordedSetsL
            %~ insertWith
              (<>)
              DrivenInsaneInvestigators
              (singleton $ recorded $ unInvestigatorId iid)
    InvestigatorKilled _ iid ->
      pure $
        a
          & logL . recordedSetsL
            %~ insertWith
              (<>)
              KilledInvestigators
              (singleton $ recorded $ unInvestigatorId iid)
    CreateWeaknessInThreatArea (PlayerCard pc) iid -> do
      pure $
        a
          & decksL
            %~ adjustMap (withDeck (pc {pcOwner = Just iid} :)) iid
    AddCardToDeckForCampaign iid pc -> do
      pure $
        a
          & decksL
            %~ adjustMap (withDeck (pc {pcOwner = Just iid} :)) iid
    RemoveCardFromDeckForCampaign iid pc ->
      pure $ a & decksL %~ adjustMap (withDeck (filter (/= pc))) iid
    _ -> pure a
