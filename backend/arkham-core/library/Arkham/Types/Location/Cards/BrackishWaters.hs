{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.BrackishWaters where

import Arkham.Import

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
  1
  (Static 0)
  Triangle
  [Squiggle, Square, Diamond, Hourglass]
  [Riverside, Bayou]

instance IsInvestigator investigator => HasModifiersFor env investigator BrackishWaters where
  getModifiersFor _ i (BrackishWaters attrs) =
    pure [ CannotPlay [AssetType] | atLocation i attrs ]

instance (IsInvestigator investigator) => HasActions env investigator BrackishWaters where
  getActions i NonFast (BrackishWaters attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    let
      assetsCount =
        count
            (maybe False (playerCardMatch (AssetType, Nothing)) . toPlayerCard)
            (handOf i)
          + inPlayAssetsCount (inPlayCounts i)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
         | atLocation i attrs
           && assetsCount
           >= 2
           && hasActionsRemaining i Nothing locationTraits
         ]
  getActions i window (BrackishWaters attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env BrackishWaters where
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
