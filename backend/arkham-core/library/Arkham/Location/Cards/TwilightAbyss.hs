module Arkham.Location.Cards.TwilightAbyss
  ( twilightAbyss
  , TwilightAbyss(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message

newtype TwilightAbyss = TwilightAbyss LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightAbyss :: LocationCard TwilightAbyss
twilightAbyss = location TwilightAbyss Cards.twilightAbyss 2 (PerPlayer 2)

instance HasAbilities TwilightAbyss where
  getAbilities (TwilightAbyss attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TwilightAbyss where
  runMessage msg (TwilightAbyss attrs) = case msg of
    RevealLocation _ lid | lid == toId attrs -> do
      TwilightAbyss <$> runMessage msg (attrs & labelL .~ "twilightAbyss")
    _ -> TwilightAbyss <$> runMessage msg attrs
