module Arkham.Homebrew.CircusExMortis.Treacheries.BalefulEclipse (balefulEclipse) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (hasSealedMoonToken)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype BalefulEclipse = BalefulEclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balefulEclipse :: TreacheryCard BalefulEclipse
balefulEclipse = treachery BalefulEclipse Cards.balefulEclipse

{- | Unrestricted on purpose: the action loss is conditional but the discard is
not, so with nobody holding a ☾ token the discard is still the state change that
makes the Forced fire.
-}
instance HasAbilities BalefulEclipse where
  getAbilities (BalefulEclipse a) = [mkAbility a 1 $ forced $ PhaseBegins #when #mythos]

instance RunMessage BalefulEclipse where
  runMessage msg t@(BalefulEclipse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "that many fewer actions to take during that round" (RR, "Action"): the
      -- round's actions are set by `Do BeginRound`, which runs ahead of
      -- `Begin MythosPhase`, so a plain loss here comes off this round's three.
      selectEach hasSealedMoonToken \iid -> loseActions iid (attrs.ability 1) 1
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> BalefulEclipse <$> liftRunMessage msg attrs
