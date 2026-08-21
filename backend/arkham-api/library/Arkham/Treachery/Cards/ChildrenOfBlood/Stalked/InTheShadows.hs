module Arkham.Treachery.Cards.ChildrenOfBlood.Stalked.InTheShadows (inTheShadows) where

import Arkham.Matcher
import Arkham.Trait (Trait (Creature, Monster))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Stalked qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InTheShadows = InTheShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadows :: TreacheryCard InTheShadows
inTheShadows = treachery InTheShadows Cards.inTheShadows

instance RunMessage InTheShadows where
  runMessage msg t@(InTheShadows attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      findAndDrawEncounterCard iid
        $ #enemy
        <> CardWithOneOf [CardWithTrait Creature, CardWithTrait Monster]
      pure t
    _ -> InTheShadows <$> liftRunMessage msg attrs
