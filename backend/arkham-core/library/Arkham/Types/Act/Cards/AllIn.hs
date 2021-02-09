module Arkham.Types.Act.Cards.AllIn
  ( AllIn(..)
  , allIn
  )
where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Action

newtype AllIn = AllIn ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn :: AllIn
allIn = AllIn $ baseAttrs "02068" "All In" (Act 3 A) Nothing

instance ActionRunner env => HasActions env AllIn where
  getActions iid NonFast (AllIn attrs) = withBaseActions iid NonFast attrs $ do
    investigatorLocationId <- getId @LocationId iid
    maid <- fmap unStoryAssetId <$> getId (CardCode "02080")
    case maid of
      Nothing -> pure []
      Just aid -> do
        miid <- fmap unOwnerId <$> getId aid
        assetLocationId <- getId aid
        pure
          [ ActivateCardAbilityAction
              iid
              (mkAbility
                (ProxySource (AssetSource aid) (toSource attrs))
                1
                (ActionAbility (Just Parley) $ ActionCost 1)
              )
          | isNothing miid && Just investigatorLocationId == assetLocationId
          ]
  getActions i window (AllIn x) = getActions i window x

instance ActRunner env => RunMessage env AllIn where
  runMessage msg a@(AllIn attrs@ActAttrs {..}) = case msg of
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when
        (null investigatorIds)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        $ chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
      pure $ AllIn $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      resignedWithDrFrancisMorgan <- elem (ResignedCardCode $ CardCode "02080")
        <$> getList ()
      a <$ unshiftMessage
        (ScenarioResolution $ Resolution $ if resignedWithDrFrancisMorgan
          then 2
          else 1
        )
    UseCardAbility iid (ProxySource _ source) _ 1 _
      | isSource attrs source && onSide A attrs -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "02080")
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
    PassedSkillTest iid _ source _ _ _
      | isSource attrs source && onSide A attrs -> do
        maid <- fmap unStoryAssetId <$> getId (CardCode "02080")
        case maid of
          Nothing -> error "this ability should not be able to be used"
          Just aid -> do
            currentClueCount <- unClueCount <$> getCount aid
            requiredClueCount <- getPlayerCountValue (PerPlayer 1)
            unshiftMessage (PlaceClues (AssetTarget aid) 1)
            a <$ when
              (currentClueCount + 1 >= requiredClueCount)
              (unshiftMessage $ TakeControlOfAsset iid aid)
    _ -> AllIn <$> runMessage msg attrs
