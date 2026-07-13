module Arkham.Homebrew.DarkMatter.Treacheries.FutureEvils (futureEvils) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Placement
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype FutureEvils = FutureEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

futureEvils :: TreacheryCard FutureEvils
futureEvils = treachery FutureEvils Cards.futureEvils

getPlacedDoomTarget :: [Window] -> Maybe Target
getPlacedDoomTarget [] = Nothing
getPlacedDoomTarget ((windowType -> Window.PlacedDoom _ target _) : _) = Just target
getPlacedDoomTarget (_ : rest) = getPlacedDoomTarget rest

instance HasAbilities FutureEvils where
  getAbilities (FutureEvils a) =
    [ mkAbility a 1
        $ forced
        $ PlacedDoomCounter #after (NotSource SourceIsPlayerCard) AnyTarget
    ]

instance RunMessage FutureEvils where
  runMessage msg t@(FutureEvils attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 (getPlacedDoomTarget -> Just target) _ -> do
      placeDoom (attrs.ability 1) target 1
      toDiscard (attrs.ability 1) attrs
      push AdvanceAgendaIfThresholdSatisfied
      pure t
    _ -> FutureEvils <$> liftRunMessage msg attrs
