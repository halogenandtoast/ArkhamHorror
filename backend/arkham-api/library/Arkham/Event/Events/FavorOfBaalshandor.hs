module Arkham.Event.Events.FavorOfBaalshandor (favorOfBaalshandor) where

import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (withAdditionalResources)
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Matcher hiding (PlayCard)
import Arkham.Window (defaultWindows)

newtype FavorOfBaalshandor = FavorOfBaalshandor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

favorOfBaalshandor :: EventCard FavorOfBaalshandor
favorOfBaalshandor = event FavorOfBaalshandor Cards.favorOfBaalshandor

instance RunMessage FavorOfBaalshandor where
  runMessage msg e@(FavorOfBaalshandor attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let windows'' = nub $ attrs.windows <> defaultWindows iid
      cards <- withAdditionalResources iid 3 do
        filterM
          (getIsPlayable iid GameSource (UnpaidCost NoAction) windows'')
          =<< select (inHandOf NotForPlay iid <> basic (#asset <> oneOf [#spell, #ritual]))
      focusCards cards do
        chooseTargetM iid cards \card -> do
          unfocusCards
          reduceCostOf attrs card 3
          playCardPayingCost iid card
      pure e
    _ -> FavorOfBaalshandor <$> liftRunMessage msg attrs
