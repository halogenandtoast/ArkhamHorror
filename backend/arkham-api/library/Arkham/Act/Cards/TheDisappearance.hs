module Arkham.Act.Cards.TheDisappearance (theDisappearance) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Act.Sequence
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection

newtype TheDisappearance = TheDisappearance ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDisappearance :: ActCard TheDisappearance
theDisappearance = act (1, A) TheDisappearance Cards.theDisappearance Nothing

instance HasAbilities TheDisappearance where
  getAbilities (TheDisappearance a) =
    [mkAbility a 1 $ forced $ InvestigatorEliminated #when You]

instance RunMessage TheDisappearance where
  runMessage msg a@(TheDisappearance attrs) = runQueueT $ case msg of
    KonamiCode -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Note, you can't actually trigger this
      record YouAreBeingHunted
      push $ RevertAct attrs.id
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeAllClues (attrs.ability 1) iid
      placeClues (attrs.ability 1) attrs =<< field InvestigatorClues iid
      pure a
    RevertAct (isSide B attrs -> True) -> do
      pure $ TheDisappearance $ attrs & (sequenceL .~ Sequence 1 A)
    _ -> TheDisappearance <$> liftRunMessage msg attrs
