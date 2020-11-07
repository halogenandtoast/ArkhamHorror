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
  MagnifyingGlass1 $ baseAttrs uuid "01040" $ slots .= [HandSlot]

instance HasModifiersFor env MagnifyingGlass1 where
  getModifiersFor _ (InvestigatorTarget iid) (MagnifyingGlass1 a) =
    pure
      [ ActionSkillModifier Action.Investigate SkillIntellect 1
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MagnifyingGlass1 where
  getActions iid _ (MagnifyingGlass1 a) | ownedBy a iid = do
    locationId <- asks $ getId @LocationId iid
    clueCount' <- asks $ unClueCount . getCount locationId
    pure [ UseCardAbility iid (toSource a) Nothing 1 | clueCount' == 0 ]
  getActions i window (MagnifyingGlass1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage (ReturnToHand iid (toTarget attrs))
    _ -> MagnifyingGlass1 <$> runMessage msg attrs
