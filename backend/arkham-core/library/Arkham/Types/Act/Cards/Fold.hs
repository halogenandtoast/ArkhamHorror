{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.Fold
  ( Fold(..)
  , fold
  )
where

import Arkham.Import hiding (fold)

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Action

newtype Fold = Fold Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fold :: Fold
fold = Fold $ baseAttrs "02069" "Fold" (Act 3 A) Nothing

instance ActionRunner env => HasActions env Fold where
  getActions iid NonFast (Fold attrs) = do
    baseActions <- getActions iid NonFast attrs
    investigatorLocationId <- getId @LocationId iid
    maid <- fmap unStoryAssetId <$> getId (CardCode "02079")
    case maid of
      Nothing -> pure baseActions
      Just aid -> do
        miid <- fmap unOwnerId <$> getId aid
        assetLocationId <- getId aid
        hasParleyActionsRemaining <- getCanAffordCost
          iid
          (toSource attrs)
          (ActionCost 1 (Just Parley) mempty)
        pure
          $ baseActions
          <> [ ActivateCardAbilityAction
                 iid
                 (mkAbility
                   (ProxySource (AssetSource aid) (toSource attrs))
                   1
                   (ActionAbility 1 (Just Parley))
                 )
             | isNothing miid
               && Just investigatorLocationId
               == assetLocationId
               && hasParleyActionsRemaining
             ]
  getActions i window (Fold x) = getActions i window x

instance ActRunner env => RunMessage env Fold where
  runMessage msg a@(Fold attrs@Attrs {..}) = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when
        (null investigatorIds)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      investigatorIds <- getInvestigatorIds
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ Fold $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      maid <- fmap unStoryAssetId <$> getId (CardCode "02079")
      a <$ case maid of
        Nothing -> unshiftMessage (Resolution 1)
        Just assetId -> do
          miid <- fmap unOwnerId <$> getId assetId
          unshiftMessage (maybe (Resolution 1) (const (Resolution 2)) miid)
    UseCardAbility iid (ProxySource _ source) _ 1
      | isSource attrs source && actSequence == Act 3 A -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "02079")
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> a <$ unshiftMessage
            (BeginSkillTest
              iid
              source
              (AssetTarget aid)
              (Just Parley)
              SkillWillpower
              3
            )
    PassedSkillTest iid _ source _ _
      | isSource attrs source && actSequence == Act 3 A -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "02079")
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> do
            currentClueCount <- unClueCount <$> getCount aid
            requiredClueCount <- getPlayerCountValue (PerPlayer 1)
            unshiftMessage (PlaceClues (AssetTarget aid) 1)
            a <$ when
              (currentClueCount + 1 >= requiredClueCount)
              (unshiftMessage $ TakeControlOfAsset iid aid)
    _ -> Fold <$> runMessage msg attrs
