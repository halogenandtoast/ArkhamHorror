module Arkham.Location.Cards.GrandEntryway (grandEntryway, GrandEntryway (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Cave))

newtype GrandEntryway = GrandEntryway LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandEntryway :: LocationCard GrandEntryway
grandEntryway = location GrandEntryway Cards.grandEntryway 1 (Static 0)

instance HasModifiersFor GrandEntryway where
  getModifiersFor (LocationTarget lid) (GrandEntryway attrs) = do
    toModifiers attrs
      $ if lid == attrs.id
        then [ConnectedToWhen (LocationWithId attrs.id) (LocationWithTrait Cave)]
        else [ConnectedToWhen (LocationWithTrait Cave) (LocationWithId attrs.id)]
  getModifiersFor _ _ = pure []

instance HasAbilities GrandEntryway where
  getAbilities (GrandEntryway attrs) =
    extendRevealed attrs []

instance RunMessage GrandEntryway where
  runMessage msg (GrandEntryway attrs) = runQueueT $ case msg of
    _ -> GrandEntryway <$> liftRunMessage msg attrs
