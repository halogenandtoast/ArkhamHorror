{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Campaign.Runner
  ( module X
  ) where

import Arkham.Prelude

import Arkham.Classes.GameLogger
import Arkham.Campaign.Types as X
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message
import Arkham.Name
import Data.HashMap.Strict qualified as HashMap

instance RunMessage CampaignAttrs where
  runMessage msg a@CampaignAttrs {..} = case msg of
    StartCampaign -> a <$ push (CampaignStep campaignStep)
    CampaignStep Nothing -> a <$ push GameOver -- TODO: move to generic
    CampaignStep (Just (ScenarioStep sid)) -> do
      a <$ pushAll [ResetInvestigators, ResetGame, StartScenario sid]
    CampaignStep (Just (UpgradeDeckStep _)) -> do
      investigatorIds <- allInvestigatorIds
      a <$ pushAll
        (ResetGame
        : map chooseUpgradeDeck investigatorIds
        <> [FinishedUpgradingDecks]
        )
    SetTokensForScenario -> a <$ push (SetTokens campaignChaosBag)
    AddCampaignCardToDeck iid cardDef -> do
      card <- lookupPlayerCard cardDef <$> getRandom
      pure $ a & storyCardsL %~ insertWith
        (<>)
        iid
        [card { pcOwner = Just iid }]
    RemoveCampaignCard cardDef -> do
      pure
        $ a
        & storyCardsL
        %~ HashMap.map (filter ((/= cardDef) . toCardDef))
        & decksL
        %~ HashMap.map (withDeck (filter ((/= cardDef) . toCardDef)))
    RemoveCampaignCardFromDeck iid cardDef ->
      pure
        $ a
        & storyCardsL
        %~ adjustMap (filter ((/= cardDef) . toCardDef)) iid
        & decksL
        %~ adjustMap (withDeck (filter ((/= cardDef) . toCardDef))) iid
    AddToken token -> pure $ a & chaosBagL %~ (token :)
    RemoveAllTokens token -> pure $ a & chaosBagL %~ filter (/= token)
    InitDeck iid deck -> do
      (deck', randomWeaknesses) <- addRandomBasicWeaknessIfNeeded deck
      let
        mentalTrauma = getSum $ foldMap
          (Sum . fromMaybe 0 . cdPurchaseMentalTrauma . toCardDef)
          (unDeck deck')
      pushAll
        $ map (AddCampaignCardToDeck iid) randomWeaknesses
        <> [ SufferTrauma iid 0 mentalTrauma | mentalTrauma > 0 ]

      pure $ a & decksL %~ insertMap iid deck'
    UpgradeDeck iid deck -> do
      let
        oldDeck = fromJustNote "No deck?" $ lookup iid campaignDecks
        deckDiff = foldr
          (\x -> deleteFirstMatch ((== toCardCode x) . toCardCode))
          (unDeck deck)
          (unDeck oldDeck)
        mentalTrauma = getSum $ foldMap
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
          if key `member` view (logL . recorded) a then insertSet key else id
        removeOrderedKey =
          if key `member` view (logL . recorded) a then filter (/= key) else id

      pure
        $ a
        & (logL . recorded %~ deleteSet key)
        & (logL . crossedOut %~ crossedOutModifier)
        & (logL . recordedSets %~ deleteMap key)
        & (logL . recordedCounts %~ deleteMap key)
        & (logL . orderedKeys %~ removeOrderedKey)
    Record key -> do
      send $ "Record \"" <> format key <> "\""
      pure $ a & logL . recorded %~ insertSet key & logL . orderedKeys %~ (<> [key])
    RecordSet key cardCodes ->
      pure $ a & logL . recordedSets %~ insertMap key (map Recorded cardCodes)
    RecordSetInsert key cardCodes -> do
      let defs = mapMaybe lookupCardDef cardCodes
      for_ defs $ \def ->
        send $ "Record \"" <> format (toName def) <> " " <> format key <> "\""
      pure $ case a ^. logL . recordedSets . at key of
        Nothing ->
          a & logL . recordedSets %~ insertMap key (map Recorded cardCodes)
        Just set ->
          let
            set' =
              filter ((`notElem` cardCodes) . unrecorded) set
                <> map Recorded cardCodes
          in a & logL . recordedSets %~ insertMap key set'
    CrossOutRecordSetEntries key cardCodes ->
      pure
        $ a
        & logL
        . recordedSets
        %~ adjustMap
             (map
               (\case
                 Recorded c | c `elem` cardCodes -> CrossedOut c
                 other -> other
               )
             )
             key
    RecordCount key int ->
      pure $ a & logL . recordedCounts %~ insertMap key int
    ScenarioResolution r -> case campaignStep of
      Just (ScenarioStep sid) -> pure $ a & resolutionsL %~ insertMap sid r
      _ -> error "must be called in a scenario"
    DrivenInsane iid -> pure $ a & logL . recordedSets %~ insertWith
      (<>)
      DrivenInsaneInvestigators
      (singleton $ Recorded $ unInvestigatorId iid)
    InvestigatorKilled _ iid -> pure $ a & logL . recordedSets %~ insertWith
      (<>)
      KilledInvestigators
      (singleton $ Recorded $ unInvestigatorId iid)
    CreateWeaknessInThreatArea (PlayerCard pc) iid -> do
      pure
        $ a
        & decksL
        %~ adjustMap (withDeck (pc { pcOwner = Just iid } :)) iid
    AddCardToDeckForCampaign iid pc -> do
      pure
        $ a
        & decksL
        %~ adjustMap (withDeck (pc { pcOwner = Just iid } :)) iid
    RemoveCardFromDeckForCampaign iid pc ->
      pure $ a & decksL %~ adjustMap (withDeck (filter (/= pc))) iid
    _ -> pure a
