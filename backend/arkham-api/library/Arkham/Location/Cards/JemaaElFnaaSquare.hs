module Arkham.Location.Cards.JemaaElFnaaSquare (jemaaElFnaaSquare) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Search
import Arkham.Strategy
import Arkham.Token

newtype JemaaElFnaaSquare = JemaaElFnaaSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jemaaElFnaaSquare :: LocationCard JemaaElFnaaSquare
jemaaElFnaaSquare = symbolLabel $ location JemaaElFnaaSquare Cards.jemaaElFnaaSquare 3 (PerPlayer 3)

instance HasAbilities JemaaElFnaaSquare where
  getAbilities (JemaaElFnaaSquare a) =
    if a.revealed
      then
        extendRevealed
          a
          [ playerLimit PerGame
              $ restricted
                a
                1
                ( Here
                    <> thisExists a (LocationWithToken Civilian)
                    <> youExist (can.reveal.cards <> can.manipulate.deck)
                )
                actionAbility
          , becomeAbandonedAbility a 2
          ]
      else extendUnrevealed1 a $ becomeAbandonedAbility a 1

instance RunMessage JemaaElFnaaSquare where
  runMessage msg l@(JemaaElFnaaSquare attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 | attrs.unrevealed -> do
      swapLocation attrs =<< fetchCard Cards.jemaaElFnaaSquareAbandoned
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealingEdit iid (attrs.ability 1) iid (FromTopOfDeck 6) \s ->
        s
          { searchZones = [fromTopOfDeck 6]
          , searchFoundStrategy = PlayFound iid 1
          , searchMatcher = #asset
          }
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      swapLocation attrs =<< fetchCard Cards.jemaaElFnaaSquareAbandoned
      pure l
    _ -> JemaaElFnaaSquare <$> liftRunMessage msg attrs
