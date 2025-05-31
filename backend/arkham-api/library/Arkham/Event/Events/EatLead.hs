module Arkham.Event.Events.EatLead (eatLead) where

import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Modifier

newtype EatLead = EatLead EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eatLead :: EventCard EatLead
eatLead = event EatLead Cards.eatLead

instance RunMessage EatLead where
  runMessage msg e@(EatLead attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      for_ (getWindowAsset attrs.windows) \aid -> do
        spendUses attrs aid Ammo 1
        nextSkillTestModifier iid attrs iid (DrawAdditionalChaosTokens 1)
        cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> EatLead <$> liftRunMessage msg attrs
