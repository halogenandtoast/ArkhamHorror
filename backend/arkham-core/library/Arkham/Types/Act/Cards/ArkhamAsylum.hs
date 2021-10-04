module Arkham.Types.Act.Cards.ArkhamAsylum
  ( ArkhamAsylum(..)
  , arkhamAsylum
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher

newtype ArkhamAsylum = ArkhamAsylum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum = act
  (1, A)
  ArkhamAsylum
  Cards.arkhamAsylum
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance ActRunner env => RunMessage env ArkhamAsylum where
  runMessage msg (ArkhamAsylum attrs) = ArkhamAsylum <$> runMessage msg attrs
