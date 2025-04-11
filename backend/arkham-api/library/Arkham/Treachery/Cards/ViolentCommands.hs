module Arkham.Treachery.Cards.ViolentCommands (violentCommands) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ViolentCommands = ViolentCommands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violentCommands :: TreacheryCard ViolentCommands
violentCommands = treachery ViolentCommands Cards.violentCommands

instance HasAbilities ViolentCommands where
  getAbilities (ViolentCommands attrs) =
    [ mkAbility attrs 1 actionAbility
    , restricted attrs 2 InYourThreatArea $ forced $ TurnEnds #when You
    ]

instance RunMessage ViolentCommands where
  runMessage msg t@(ViolentCommands attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (colocatedWith iid) \iid' -> do
        assignDamage iid' attrs 2
        toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignHorror iid attrs 1
      pure t
    _ -> ViolentCommands <$> liftRunMessage msg attrs
