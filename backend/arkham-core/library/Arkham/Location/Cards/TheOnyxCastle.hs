module Arkham.Location.Cards.TheOnyxCastle
  ( theOnyxCastle
  , TheOnyxCastle(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheOnyxCastle = TheOnyxCastle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOnyxCastle :: LocationCard TheOnyxCastle
theOnyxCastle = location TheOnyxCastle Cards.theOnyxCastle 3 (Static 0)

instance HasAbilities TheOnyxCastle where
  getAbilities (TheOnyxCastle attrs) =
    extendRevealed attrs []

instance RunMessage TheOnyxCastle where
  runMessage msg (TheOnyxCastle attrs) = runQueueT $ case msg of
    _ -> TheOnyxCastle <$> lift (runMessage msg attrs)
