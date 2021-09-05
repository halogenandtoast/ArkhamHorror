module Arkham.Types.Act.Cards.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype CurtainCall = CurtainCall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: ActCard CurtainCall
curtainCall = act (3, A) CurtainCall Cards.curtainCall Nothing

instance ActRunner env => RunMessage env CurtainCall where
  runMessage msg (CurtainCall attrs) = CurtainCall <$> runMessage msg attrs
