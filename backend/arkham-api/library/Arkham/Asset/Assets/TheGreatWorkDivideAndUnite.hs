module Arkham.Asset.Assets.TheGreatWorkDivideAndUnite (theGreatWorkDivideAndUnite) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype TheGreatWorkDivideAndUnite = TheGreatWorkDivideAndUnite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWorkDivideAndUnite :: AssetCard TheGreatWorkDivideAndUnite
theGreatWorkDivideAndUnite = asset TheGreatWorkDivideAndUnite Cards.theGreatWorkDivideAndUnite

instance RunMessage TheGreatWorkDivideAndUnite where
  runMessage msg a@(TheGreatWorkDivideAndUnite attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | attrs.cardId == card.id -> do
      gameModifier attrs iid (XPModifier "The Great Work" 1)
      gameModifier attrs iid BecomeHomunculusWhenDefeated
      pure a
    _ -> TheGreatWorkDivideAndUnite <$> liftRunMessage msg attrs
