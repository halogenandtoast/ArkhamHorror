{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MagnifyingGlass1 where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype MagnifyingGlass1 = MagnifyingGlass1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass1 :: AssetId -> MagnifyingGlass1
magnifyingGlass1 uuid =
  MagnifyingGlass1 $ (baseAttrs uuid "01040") { assetSlots = [HandSlot] }

instance IsInvestigator investigator => HasModifiersFor env investigator MagnifyingGlass1 where
  getModifiersFor _ i (MagnifyingGlass1 a) = pure
    [ ActionSkillModifier Action.Investigate SkillIntellect 1 | ownedBy a i ]

instance (ActionRunner env investigator) => HasActions env investigator MagnifyingGlass1 where
  getActions i _ (MagnifyingGlass1 a) | ownedBy a i = do
    clueCount' <- asks $ unClueCount . getCount (locationOf i)
    pure
      [ UseCardAbility (getId () i) (toSource a) Nothing 1 | clueCount' == 0 ]
  getActions i window (MagnifyingGlass1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage (ReturnToHand iid (toTarget attrs))
    _ -> MagnifyingGlass1 <$> runMessage msg attrs
