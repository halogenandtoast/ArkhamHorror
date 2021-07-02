module Arkham.Types.Location.Cards.Bathroom where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bathroom)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Token

newtype Bathroom = Bathroom LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathroom :: LocationId -> Bathroom
bathroom = Bathroom . baseAttrs
  Cards.bathroom
  1
  (PerPlayer 1)
  Star
  [T]

instance HasModifiersFor env Bathroom where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Bathroom where
  getActions i window (Bathroom attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Bathroom where
  runMessage msg l@(Bathroom attrs) = case msg of
    After (RevealToken (SkillTestSource _ _ source _ (Just Action.Investigate)) iid tokenFace)
      | isSource attrs source
      -> l <$ when
        (tokenFace `elem` [Skull, Cultist, Tablet, AutoFail])
        (unshiftMessages [SetActions iid (toSource attrs) 0, ChooseEndTurn iid])
    _ -> Bathroom <$> runMessage msg attrs
