module Arkham.Event.Events.Prestidigitation (prestidigitation, prestidigitationEffect) where

import Arkham.Cost.Status
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Prestidigitation = Prestidigitation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prestidigitation :: EventCard Prestidigitation
prestidigitation = event Prestidigitation Cards.prestidigitation

instance RunMessage Prestidigitation where
  runMessage msg e@(Prestidigitation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ PlayableCardWithCostReduction NoAction 2 $ inHandOf ForPlay iid <> basic #item
      chooseTargetM iid cards \card -> do
        reduceCostOf attrs card 2
        playCardPayingCostWithWindows iid card attrs.windows
        createCardEffect Cards.prestidigitation Nothing attrs iid
      pure e
    _ -> Prestidigitation <$> liftRunMessage msg attrs

newtype PrestidigitationEffect = PrestidigitationEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prestidigitationEffect :: EffectArgs -> PrestidigitationEffect
prestidigitationEffect = cardEffect PrestidigitationEffect Cards.prestidigitation

instance RunMessage PrestidigitationEffect where
  runMessage msg e@(PrestidigitationEffect attrs) = runQueueT $ case msg of
    EndTurn _ -> do
      for_ attrs.target.investigator \iid -> do
        items <- select $ assetControlledBy iid <> #item
        chooseTargetM iid items $ returnToHand iid
      disableReturn e
    _ -> PrestidigitationEffect <$> liftRunMessage msg attrs
