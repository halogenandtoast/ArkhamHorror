module Arkham.Location.Cards.LoadingDocks (loadingDocks) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LoadingDocks = LoadingDocks LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDocks :: LocationCard LoadingDocks
loadingDocks = location LoadingDocks Cards.loadingDocks 4 (PerPlayer 2)

instance HasModifiersFor LoadingDocks where
  getModifiersFor (LoadingDocks a) = when (a.token #resource == 0) do
    modifySelect a Anyone [CannotDiscoverCluesAt (be a)]

instance HasAbilities LoadingDocks where
  getAbilities (LoadingDocks a) =
    extendRevealed1 a
      $ restricted a 1 (thisExists a $ LocationWithResources (atLeast 1))
      $ forced
      $ RoundEnds #when

instance RunMessage LoadingDocks where
  runMessage msg l@(LoadingDocks attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 1) attrs #resource 1
      pure l
    _ -> LoadingDocks <$> liftRunMessage msg attrs
