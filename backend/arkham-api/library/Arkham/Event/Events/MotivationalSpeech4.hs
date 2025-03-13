{-# OPTIONS_GHC -Wno-deprecations #-}
module Arkham.Event.Events.MotivationalSpeech4 (motivationalSpeech4) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Ability
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype MotivationalSpeech4 = MotivationalSpeech4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motivationalSpeech4 :: EventCard MotivationalSpeech4
motivationalSpeech4 = event MotivationalSpeech4 Cards.motivationalSpeech4

instance RunMessage MotivationalSpeech4 where
  runMessage msg e@(MotivationalSpeech4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      ts <- select $ affectsOthers $ colocatedWith iid
      chooseTargetM iid ts (handleTarget iid attrs)
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      allies <- select $ PlayableCardWithNoCost NoAction $ inHandOf ForPlay iid <> #ally
      when (notNull allies) do
        focusCards allies do
          chooseOneM iid do
            labeled "Do not play ally" unfocusCards
            targets allies \ally -> do
              unfocusCards
              putCardIntoPlay iid ally
              forTarget_ ally msg
      pure e
    ForTarget
      (CardIdTarget cid)
      (HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid)) -> do
        abilities <-
          selectMap (ignoreAllCosts . noAOO)
            $ AssetAbility (AssetWithCardId cid)
            <> oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
        abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) (traceShowId abilities)
        unless (null abilities') do
          chooseOneM iid do
            labeled "Do not trigger ability" nothing
            for_ abilities' \ab -> abilityLabeled iid ab nothing
        pure e
    _ -> MotivationalSpeech4 <$> liftRunMessage msg attrs
