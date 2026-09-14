module Arkham.Homebrew.CircusExMortis.Treacheries.DenseTangle (denseTangle) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype DenseTangle = DenseTangle TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denseTangle :: TreacheryCard DenseTangle
denseTangle = treachery DenseTangle Cards.denseTangle

instance HasAbilities DenseTangle where
  getAbilities (DenseTangle a) =
    -- Moving along the leftmost connection is the way through the tangle, so the ability
    -- never triggers on that move; ThatLocation is the location you moved from.
    [ restricted a 1 (youExist $ InvestigatorWithActionsRemaining (atLeast 1))
        $ forced
        $ Moves #after You AnySource Anywhere (not_ $ LeftmostConnectionOf ThatLocation)
    , limited (MaxPer Cards.denseTangle PerRound 1) $ mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage DenseTangle where
  runMessage msg t@(DenseTangle attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid attrs 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> DenseTangle <$> liftRunMessage msg attrs
