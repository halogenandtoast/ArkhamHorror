module Arkham.Treachery.Cards.VisionsInYourMindHatred (visionsInYourMindHatred) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VisionsInYourMindHatred = VisionsInYourMindHatred TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsInYourMindHatred :: TreacheryCard VisionsInYourMindHatred
visionsInYourMindHatred = treachery VisionsInYourMindHatred Cards.visionsInYourMindHatred

instance HasAbilities VisionsInYourMindHatred where
  getAbilities (VisionsInYourMindHatred a) =
    [restricted a 1 InYourHand $ forced $ TurnEnds #when (You <> not_ (TakenActionThisTurn #draw))]

instance RunMessage VisionsInYourMindHatred where
  runMessage msg t@(VisionsInYourMindHatred attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      directDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    _ -> VisionsInYourMindHatred <$> liftRunMessage msg attrs
