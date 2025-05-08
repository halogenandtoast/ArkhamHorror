module Arkham.Treachery.Cards.VisionsInYourMindDeath (visionsInYourMindDeath) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VisionsInYourMindDeath = VisionsInYourMindDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsInYourMindDeath :: TreacheryCard VisionsInYourMindDeath
visionsInYourMindDeath = treachery VisionsInYourMindDeath Cards.visionsInYourMindDeath

instance HasAbilities VisionsInYourMindDeath where
  getAbilities (VisionsInYourMindDeath a) = [restricted a 1 InYourHand $ forced $ TurnEnds #when (You <> not_ (TakenActionThisTurn #move))]

instance RunMessage VisionsInYourMindDeath where
  runMessage msg t@(VisionsInYourMindDeath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      directDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    _ -> VisionsInYourMindDeath <$> liftRunMessage msg attrs
