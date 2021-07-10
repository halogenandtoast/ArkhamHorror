module Arkham.Types.Asset.Cards.Adaptable1
  ( adaptable1
  , Adaptable1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Id

newtype Adaptable1 = Adaptable1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adaptable1 :: AssetCard Adaptable1
adaptable1 = asset Adaptable1 Cards.adaptable1

instance HasAbilities Adaptable1

instance HasModifiersFor env Adaptable1

instance (HasSet InvestigatorId env (), HasQueue env, HasModifiersFor env ()) => RunMessage env Adaptable1 where
  runMessage msg (Adaptable1 attrs) = Adaptable1 <$> runMessage msg attrs
