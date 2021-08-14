module Arkham.Types.Location.Cards.Bathroom where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (bathroom)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Token

newtype Bathroom = Bathroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathroom :: LocationCard Bathroom
bathroom = location Bathroom Cards.bathroom 1 (PerPlayer 1) Star [T]

instance HasModifiersFor env Bathroom

instance ActionRunner env => HasAbilities env Bathroom where
  getAbilities i window (Bathroom attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env Bathroom where
  runMessage msg l@(Bathroom attrs) = case msg of
    After (RevealToken (SkillTestSource _ _ source _ (Just Action.Investigate)) iid token)
      | isSource attrs source
      -> l <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, AutoFail])
        (pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid])
    _ -> Bathroom <$> runMessage msg attrs
