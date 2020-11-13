{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.BrackishWaters where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BrackishWaters = BrackishWaters Attrs
  deriving newtype (Show, ToJSON, FromJSON)

brackishWaters :: BrackishWaters
brackishWaters = BrackishWaters $ baseAttrs
  "81010"
  "Brackish Waters"
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Triangle
  [Squiggle, Square, Diamond, Hourglass]
  [Riverside, Bayou]

instance HasModifiersFor env BrackishWaters where
  getModifiersFor _ (InvestigatorTarget iid) (BrackishWaters attrs) =
    pure [ CannotPlay [AssetType] | iid `elem` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

-- TODO: LEFT OFF HERE WITH HAND OF
instance ActionRunner env => HasActions env BrackishWaters where
  getActions iid NonFast (BrackishWaters attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    hand <- getHandOf iid
    inPlayAssetsCount <- getInPlayOf iid <&> count
      (\case
        PlayerCard pc -> pcCardType pc == AssetType
        EncounterCard _ -> False
      )
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList locationTraits)
    let
      assetsCount =
        count
            (maybe False (playerCardMatch (AssetType, Nothing)) . toPlayerCard)
            hand
          + inPlayAssetsCount
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
         | iid
           `member` locationInvestigators
           && assetsCount
           >= 2
           && hasActionsRemaining
         ]
  getActions i window (BrackishWaters attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env BrackishWaters where
  runMessage msg l@(BrackishWaters attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      assetIds <- asks $ setToList . getSet @AssetId iid
      handAssetIds <- asks $ map unHandCardId . setToList . getSet
        (iid, AssetType)
      l <$ unshiftMessages
        [ Ask
          iid
          (ChooseN 2
          $ [ Discard (AssetTarget aid) | aid <- assetIds ]
          <> [ DiscardCard iid cid | cid <- handAssetIds ]
          )
        , BeginSkillTest
          iid
          source
          (toTarget attrs)
          Nothing
          SkillAgility
          3
          [TakeControlOfSetAsideAsset iid "81021"]
          mempty
          mempty
          mempty
        ]
    _ -> BrackishWaters <$> runMessage msg attrs
