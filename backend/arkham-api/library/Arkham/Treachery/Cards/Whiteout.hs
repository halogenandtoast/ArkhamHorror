module Arkham.Treachery.Cards.Whiteout (whiteout, Whiteout (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Whiteout = Whiteout TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whiteout :: TreacheryCard Whiteout
whiteout = treachery Whiteout Cards.whiteout

instance HasModifiersFor Whiteout where
  getModifiersFor (InvestigatorTarget iid) (Whiteout attrs) = do
    affected <- iid <=~> InvestigatorAt (locationWithTreachery $ toId attrs)
    toModifiers attrs [AnySkillValue (-1) | affected]
  getModifiersFor _ _ = pure []

instance HasAbilities Whiteout where
  getAbilities (Whiteout a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage Whiteout where
  runMessage msg t@(Whiteout attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Whiteout <$> liftRunMessage msg attrs
