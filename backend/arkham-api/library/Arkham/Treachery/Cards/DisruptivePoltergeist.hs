module Arkham.Treachery.Cards.DisruptivePoltergeist (disruptivePoltergeist) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DisruptivePoltergeist = DisruptivePoltergeist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptivePoltergeist :: TreacheryCard DisruptivePoltergeist
disruptivePoltergeist = treachery DisruptivePoltergeist Cards.disruptivePoltergeist

instance HasAbilities DisruptivePoltergeist where
  getAbilities (DisruptivePoltergeist a) =
    [ restricted
        a
        1
        (InThreatAreaOf You <> youExist (not_ SuccessfullyEvadedThisRound))
        $ forced
        $ TurnEnds #when You
    , restricted a 2 OnSameLocation evadeAction_
    ]

instance RunMessage DisruptivePoltergeist where
  runMessage msg t@(DisruptivePoltergeist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (AnySkillValue (-2))
      chooseEvadeEnemy sid iid (attrs.ability 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> DisruptivePoltergeist <$> liftRunMessage msg attrs
