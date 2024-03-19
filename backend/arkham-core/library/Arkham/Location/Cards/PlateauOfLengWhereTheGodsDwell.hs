module Arkham.Location.Cards.PlateauOfLengWhereTheGodsDwell
  ( plateauOfLengWhereTheGodsDwell
  , PlateauOfLengWhereTheGodsDwell(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PlateauOfLengWhereTheGodsDwell = PlateauOfLengWhereTheGodsDwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLengWhereTheGodsDwell :: LocationCard PlateauOfLengWhereTheGodsDwell
plateauOfLengWhereTheGodsDwell = location PlateauOfLengWhereTheGodsDwell Cards.plateauOfLengWhereTheGodsDwell 3 (PerPlayer 1)

instance HasAbilities PlateauOfLengWhereTheGodsDwell where
  getAbilities (PlateauOfLengWhereTheGodsDwell attrs) =
    extendRevealed attrs []

instance RunMessage PlateauOfLengWhereTheGodsDwell where
  runMessage msg (PlateauOfLengWhereTheGodsDwell attrs) = runQueueT $ case msg of
    _ -> PlateauOfLengWhereTheGodsDwell <$> lift (runMessage msg attrs)
