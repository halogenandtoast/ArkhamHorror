module Arkham.Treachery.Cards.ArcaneLock (arcaneLock) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneLock = ArcaneLock TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneLock :: TreacheryCard ArcaneLock
arcaneLock = treachery ArcaneLock Cards.arcaneLock

instance HasModifiersFor ArcaneLock where
  getModifiersFor (ArcaneLock attrs) = case attrs.attached.location of
    Just lid ->
      modified_ attrs lid [AdditionalCostToEnter (ActionCost 1), AdditionalCostToLeave (ActionCost 1)]
    _ -> pure mempty

instance HasAbilities ArcaneLock where
  getAbilities (ArcaneLock a) = case a.attached.location of
    Just lid -> [skillTestAbility $ restricted a 1 (OnLocation $ LocationWithId lid) actionAbility]
    _ -> []

instance RunMessage ArcaneLock where
  runMessage msg t@(ArcaneLock attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \location -> do
        withoutArcaneLock <- selectNone $ treacheryAt location <> treacheryIs Cards.arcaneLock
        when withoutArcaneLock $ attachTreachery attrs location
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#intellect, #willpower] (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ArcaneLock <$> liftRunMessage msg attrs
