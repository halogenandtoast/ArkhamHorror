module Arkham.Location.Cards.VipArea
  ( vipArea
  , VipArea(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (vipArea)
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Modifier
import Arkham.Phase
import Arkham.Target

newtype VipArea = VipArea LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vipArea :: LocationCard VipArea
vipArea = locationWith
  VipArea
  Cards.vipArea
  3
  (PerPlayer 1)
  T
  [Diamond]
  (revealedSymbolL .~ Plus)

instance HasPhase env => HasModifiersFor env VipArea where
  getModifiersFor _ (InvestigatorTarget iid) (VipArea attrs)
    | iid `member` locationInvestigators attrs = do
      phase <- getPhase
      if phase == UpkeepPhase
        then pure $ toModifiers attrs [CannotDrawCards, CannotGainResources]
        else pure []
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage VipArea where
  runMessage msg (VipArea attrs) = VipArea <$> runMessage msg attrs
