module Arkham.Treachery.Cards.CaughtInAWeb (
  caughtInAWeb,
  CaughtInAWeb (..),
)
where

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtInAWeb = CaughtInAWeb TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtInAWeb :: TreacheryCard CaughtInAWeb
caughtInAWeb = treachery CaughtInAWeb Cards.caughtInAWeb

instance HasModifiersFor CaughtInAWeb where
  getModifiersFor (InvestigatorTarget iid) (CaughtInAWeb attrs) | treacheryOnInvestigator iid attrs = do
    alreadyMoved <- fieldMap InvestigatorActionsTaken (any (elem #move)) iid
    pure $ toModifiers attrs $ SkillModifier #agility (-1) : [CannotTakeAction #move | alreadyMoved]
  getModifiersFor _ _ = pure []

instance RunMessage CaughtInAWeb where
  runMessage msg t@(CaughtInAWeb attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #combat 3
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CaughtInAWeb <$> lift (runMessage msg attrs)
