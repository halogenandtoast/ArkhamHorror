module Arkham.Asset.Cards.StickToThePlan
  ( stickToThePlan
  , StickToThePlan(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Attrs
import Arkham.Asset.Runner
import Arkham.Card.PlayerCard

newtype Metadata = Metadata { attachedCards :: [PlayerCard] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype StickToThePlan = StickToThePlan (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickToThePlan :: AssetCard StickToThePlan
stickToThePlan =
  asset (StickToThePlan . (`with` Metadata [])) Cards.stickToThePlan

instance AssetRunner env => RunMessage env StickToThePlan where
  runMessage msg (StickToThePlan (attrs `With` metadata)) = StickToThePlan . (`with` metadata) <$> runMessage msg attrs
