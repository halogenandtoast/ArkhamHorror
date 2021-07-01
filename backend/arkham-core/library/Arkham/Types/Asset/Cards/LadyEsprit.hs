module Arkham.Types.Asset.Cards.LadyEsprit
  ( LadyEsprit(..)
  , ladyEsprit
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = allyWith LadyEsprit Cards.ladyEsprit (2, 4) (isStoryL .~ True)

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
        (CheckDefeated source)
        (LadyEsprit $ attrs & exhaustedL .~ True & sanityDamageL +~ 1)
    _ -> LadyEsprit <$> runMessage msg attrs
