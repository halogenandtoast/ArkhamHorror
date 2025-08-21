module Arkham.Event.Events.EverVigilant1 (everVigilant1) where

import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), withModifiersOf)
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Window

newtype EverVigilant1 = EverVigilant1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant1 :: EventCard EverVigilant1
everVigilant1 = event EverVigilant1 Cards.everVigilant1

instance RunMessage EverVigilant1 where
  runMessage msg e@(EverVigilant1 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 3 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      let windows'' = nub $ attrs.windows <> [mkWhen (DuringTurn iid), mkWhen NonFast]
      cards <- withModifiersOf iid attrs [ReduceCostOf AnyCard 1] do
        filterM (getIsPlayable iid GameSource (UnpaidCost NoAction) windows'')
          =<< select (InHandOf ForPlay (be iid) <> #asset)
      when (notNull cards) do
        chooseUpToNM iid 1 "Do not play asset" do
          targets cards \c -> do
            reduceCostOf attrs c 1
            playCardPayingCostWithWindows iid c windows''
        doStep (n - 1) msg'
      pure e
    _ -> EverVigilant1 <$> liftRunMessage msg attrs
