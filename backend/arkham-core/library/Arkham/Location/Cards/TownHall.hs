module Arkham.Location.Cards.TownHall
  ( townHall
  , TownHall(..)
  ) where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Target

newtype TownHall = TownHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

townHall :: LocationCard TownHall
townHall = location TownHall Cards.townHall 4 (PerPlayer 1)

instance HasModifiersFor TownHall where
  getModifiersFor (LocationTarget lid) (TownHall a) = do
    isDowntown <- lid <=~> locationIs Cards.downtownFirstBankOfArkham
    pure $ toModifiers
      a
      [ ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)
      | isDowntown
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities TownHall where
  getAbilities (TownHall attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TownHall where
  runMessage msg (TownHall attrs) = TownHall <$> runMessage msg attrs
