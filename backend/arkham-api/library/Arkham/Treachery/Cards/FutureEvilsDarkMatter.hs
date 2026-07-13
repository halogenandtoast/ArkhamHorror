module Arkham.Treachery.Cards.FutureEvilsDarkMatter (futureEvilsDarkMatter) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype FutureEvilsDarkMatter = FutureEvilsDarkMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

futureEvilsDarkMatter :: TreacheryCard FutureEvilsDarkMatter
futureEvilsDarkMatter = treachery FutureEvilsDarkMatter Cards.futureEvilsDarkMatter

getPlacedDoomTarget :: [Window] -> Maybe Target
getPlacedDoomTarget [] = Nothing
getPlacedDoomTarget ((windowType -> Window.PlacedDoom _ target _) : _) = Just target
getPlacedDoomTarget (_ : rest) = getPlacedDoomTarget rest

instance HasAbilities FutureEvilsDarkMatter where
  getAbilities (FutureEvilsDarkMatter a) =
    [ mkAbility a 1
        $ forced
        $ PlacedDoomCounter #after (NotSource SourceIsPlayerCard) AnyTarget
    ]

instance RunMessage FutureEvilsDarkMatter where
  runMessage msg t@(FutureEvilsDarkMatter attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (getPlacedDoomTarget -> Just target) _ -> do
      placeDoom (attrs.ability 1) target 1
      toDiscard (attrs.ability 1) attrs
      push AdvanceAgendaIfThresholdSatisfied
      pure t
    _ -> FutureEvilsDarkMatter <$> liftRunMessage msg attrs
