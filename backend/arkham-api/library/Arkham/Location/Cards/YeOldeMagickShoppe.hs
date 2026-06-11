module Arkham.Location.Cards.YeOldeMagickShoppe (yeOldeMagickShoppe) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype YeOldeMagickShoppe = YeOldeMagickShoppe LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yeOldeMagickShoppe :: LocationCard YeOldeMagickShoppe
yeOldeMagickShoppe = location YeOldeMagickShoppe Cards.yeOldeMagickShoppe 3 (PerPlayer 1)

instance HasAbilities YeOldeMagickShoppe where
  getAbilities (YeOldeMagickShoppe a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists NonEliteEnemy)
      $ FastAbility
      $ ExhaustAssetCost (AssetWithTitle "Ezra Graves" <> AssetAt (be a))
      <> ClueCost (Static 1)

instance RunMessage YeOldeMagickShoppe where
  runMessage msg l@(YeOldeMagickShoppe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select NonEliteEnemy
      chooseTargetM iid enemies \enemy -> defeatEnemy enemy iid (attrs.ability 1)
      pure l
    _ -> YeOldeMagickShoppe <$> liftRunMessage msg attrs
