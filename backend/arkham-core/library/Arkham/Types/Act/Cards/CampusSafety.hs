module Arkham.Types.Act.Cards.CampusSafety where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype CampusSafety = CampusSafety ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

campusSafety :: ActCard CampusSafety
campusSafety = act (3, A) CampusSafety Cards.campusSafety Nothing

instance ActionRunner env => HasActions env CampusSafety where
  getActions i window (CampusSafety x) = getActions i window x

instance ActRunner env => RunMessage env CampusSafety where
  runMessage msg a@(CampusSafety attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> CampusSafety <$> runMessage msg attrs
