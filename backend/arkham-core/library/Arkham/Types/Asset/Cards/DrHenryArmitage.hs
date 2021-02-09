module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetId -> DrHenryArmitage
drHenryArmitage uuid = DrHenryArmitage $ (baseAttrs uuid "02040")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

fastAbility :: AssetAttrs -> CardId -> Ability
fastAbility a cid = mkAbility
  (toSource a)
  1
  (FastAbility $ Costs [DiscardCardCost cid, ExhaustCost (toTarget a)])

instance HasModifiersFor env DrHenryArmitage where
  getModifiersFor = noModifiersFor

instance HasActions env DrHenryArmitage where
  getActions iid (AfterDrawCard You cid) (DrHenryArmitage a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid (fastAbility a cid)]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DrHenryArmitage <$> runMessage msg attrs
