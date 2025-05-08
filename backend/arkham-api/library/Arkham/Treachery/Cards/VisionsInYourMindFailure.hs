module Arkham.Treachery.Cards.VisionsInYourMindFailure (visionsInYourMindFailure) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VisionsInYourMindFailure = VisionsInYourMindFailure TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsInYourMindFailure :: TreacheryCard VisionsInYourMindFailure
visionsInYourMindFailure = treachery VisionsInYourMindFailure Cards.visionsInYourMindFailure

instance HasAbilities VisionsInYourMindFailure where
  getAbilities (VisionsInYourMindFailure a) =
    [restricted a 1 InYourHand $ forced $ TurnEnds #when (You <> not_ (TakenActionThisTurn #resource))]

instance RunMessage VisionsInYourMindFailure where
  runMessage msg t@(VisionsInYourMindFailure attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      directDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    _ -> VisionsInYourMindFailure <$> liftRunMessage msg attrs
