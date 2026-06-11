module Arkham.Location.Cards.TickTockClubFuture (tickTockClubFuture) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype TickTockClubFuture = TickTockClubFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tickTockClubFuture :: LocationCard TickTockClubFuture
tickTockClubFuture = location TickTockClubFuture Cards.tickTockClubFuture 4 (PerPlayer 2)

instance HasAbilities TickTockClubFuture where
  getAbilities (TickTockClubFuture a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost
          $ ExhaustAssetCost (AssetWithTitle "Thomas Corrigan" <> AssetAt (be a))
          <> GroupClueCost (PerPlayer 1) Anywhere
      , restricted a 2 Here
          $ actionAbilityWithCost (SpendTokenCost Token.Time (TargetIs $ toTarget a))
      ]

instance RunMessage TickTockClubFuture where
  runMessage msg l@(TickTockClubFuture attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      watchShop <- selectOne $ locationIs Cards.oMalleysWatchShop
      for_ watchShop \shop -> placeTokens (attrs.ability 1) shop Token.Time 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      agenda <- selectJust AnyAgenda
      removeDoom (attrs.ability 2) agenda 1
      pure l
    _ -> TickTockClubFuture <$> liftRunMessage msg attrs
