module Arkham.Types.Location.Cards.VipArea
  ( vipArea
  , VipArea(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (vipArea)
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Target

newtype VipArea = VipArea LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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

instance HasAbilities env VipArea where
  getAbilities iid window (VipArea attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env VipArea where
  runMessage msg (VipArea attrs) = VipArea <$> runMessage msg attrs
