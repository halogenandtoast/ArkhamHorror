module Arkham.Homebrew.CircusExMortis.Treacheries.DenseTangle (denseTangle) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window)
import Arkham.Window qualified as Window

newtype DenseTangle = DenseTangle TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denseTangle :: TreacheryCard DenseTangle
denseTangle = treachery DenseTangle Cards.denseTangle

getMoveLocations :: [Window] -> (LocationId, LocationId)
getMoveLocations [] = error "getMoveLocations: not a Moves event"
getMoveLocations ((Window.windowType -> Window.Moves _ _ (Just from) dest _) : _) = (from, dest)
getMoveLocations (_ : rest) = getMoveLocations rest

instance HasAbilities DenseTangle where
  getAbilities (DenseTangle a) =
    [ mkAbility a 1 $ forced $ Moves #after You AnySource Anywhere Anywhere
    , limited (MaxPer Cards.denseTangle PerRound 1) $ mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage DenseTangle where
  runMessage msg t@(DenseTangle attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getMoveLocations -> (from, dest)) _ -> do
      fromRevealed <- field LocationRevealed from
      fromConnections <-
        field (if fromRevealed then LocationRevealedConnectedMatchers else LocationConnectedMatchers) from
      destSymbol <- field LocationPrintedSymbol dest
      let leftmost = listToMaybe [sym | LocationWithSymbol sym <- fromConnections]
      when (leftmost /= Just destSymbol) $ loseActions iid attrs 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> DenseTangle <$> liftRunMessage msg attrs
