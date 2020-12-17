module Arkham.Types.EffectId where

import Arkham.Prelude

newtype EffectId = EffectId { unEffectId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)
