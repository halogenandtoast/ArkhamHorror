module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetId -> LadyEsprit
ladyEsprit uuid = LadyEsprit $ (baseAttrs uuid "81019")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 4
  , assetIsStory = True
  }

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1)

instance HasModifiersFor env LadyEsprit where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env LadyEsprit where
  getActions iid NonFast (LadyEsprit a@AssetAttrs {..}) = do
    locationId <- getId @LocationId iid
    assetLocationId <- case assetInvestigator of
      Nothing -> pure $ fromJustNote "must be set" assetLocation
      Just iid' -> getId iid'
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | not assetExhausted && locationId == assetLocationId
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env LadyEsprit where
  runMessage msg (LadyEsprit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessage $ chooseOne
        iid
        [HealDamage (InvestigatorTarget iid) 2, TakeResources iid 2 False]
      runMessage
        CheckDefeated
        (LadyEsprit $ attrs & exhaustedL .~ True & sanityDamageL +~ 1)
    _ -> LadyEsprit <$> runMessage msg attrs
