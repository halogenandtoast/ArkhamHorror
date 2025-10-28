module Arkham.Event.Events.EverVigilant4 (everVigilant4) where

import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), withModifiersOf)
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Window

newtype EverVigilant4 = EverVigilant4 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant4 :: EventCard EverVigilant4
everVigilant4 = event EverVigilant4 Cards.everVigilant4

instance RunMessage EverVigilant4 where
  runMessage msg e@(EverVigilant4 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 4 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      hasPlayable <- flip filterM iids \iid' -> do
        withModifiersOf iid' attrs [ReduceCostOf AnyCard 1] do
          anyM
            (getIsPlayable iid' GameSource (UnpaidCost NoAction) (defaultWindows iid'))
            =<< select (InHandOf ForPlay (be iid') <> #asset)

      when (notNull hasPlayable) do
        chooseOneM iid do
          labeled "Do not play asset" nothing
          targets hasPlayable $ handleTarget iid attrs
        doStep (n - 1) msg'
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      cards <- withModifiersOf iid attrs [ReduceCostOf AnyCard 1] do
        filterM
          (getIsPlayable iid GameSource (UnpaidCost NoAction) (defaultWindows iid))
          =<< select (InHandOf ForPlay (be iid) <> #asset)
      chooseTargetM iid cards \c -> do
        reduceCostOf attrs c 1
        playCardPayingCostWithWindows iid c (defaultWindows iid)
      pure e
    _ -> EverVigilant4 <$> liftRunMessage msg attrs
