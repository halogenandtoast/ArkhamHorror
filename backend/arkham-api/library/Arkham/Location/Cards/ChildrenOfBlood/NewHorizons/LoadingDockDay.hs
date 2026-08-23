module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.LoadingDockDay (loadingDockDay) where

import Arkham.Ability hiding (resignAction)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype LoadingDockDay = LoadingDockDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDockDay :: LocationCard LoadingDockDay
loadingDockDay = symbolLabel $ location LoadingDockDay Cards.loadingDockDay 3 (PerPlayer 1)

instance HasAbilities LoadingDockDay where
  getAbilities (LoadingDockDay a) =
    extendRevealed
      a
      [ resignAction a
      , groupLimit PerGame
          $ restricted a 1 (Here <> NoCluesOnThis)
          $ FastAbility (AddTokenCost 1 #blood)
      ]

instance RunMessage LoadingDockDay where
  runMessage msg l@(LoadingDockDay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember TheInvestigatorsFoundForgedPermits
      pure l
    _ -> LoadingDockDay <$> liftRunMessage msg attrs
