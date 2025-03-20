module Arkham.Treachery.Cards.Frenzied (frenzied) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Frenzied = Frenzied TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenzied :: TreacheryCard Frenzied
frenzied = treachery Frenzied Cards.frenzied

instance HasAbilities Frenzied where
  getAbilities (Frenzied a) =
    [ restricted
        a
        1
        (InThreatAreaOf You <> youExist (not_ SuccessfullyAttackedThisRound))
        $ forced
        $ TurnEnds #when You
    , restricted a 2 OnSameLocation fightAction_
    ]

instance RunMessage Frenzied where
  runMessage msg t@(Frenzied attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (AnySkillValue (-2))
      chooseFightEnemy sid iid (attrs.ability 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Frenzied <$> liftRunMessage msg attrs
