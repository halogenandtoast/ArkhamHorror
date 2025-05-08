module Arkham.Treachery.Cards.VisionsInYourMindHorrors (visionsInYourMindHorrors) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VisionsInYourMindHorrors = VisionsInYourMindHorrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsInYourMindHorrors :: TreacheryCard VisionsInYourMindHorrors
visionsInYourMindHorrors = treachery VisionsInYourMindHorrors Cards.visionsInYourMindHorrors

instance HasAbilities VisionsInYourMindHorrors where
  getAbilities (VisionsInYourMindHorrors a) = [restricted a 1 InYourHand $ forced $ TurnEnds #when (You <> not_ (TakenActionThisTurn #play))]

instance RunMessage VisionsInYourMindHorrors where
  runMessage msg t@(VisionsInYourMindHorrors attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      directDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    _ -> VisionsInYourMindHorrors <$> liftRunMessage msg attrs
