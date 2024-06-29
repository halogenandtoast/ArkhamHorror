module Arkham.Treachery.Cards.ArmInjury (armInjury, ArmInjury (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ActionTarget (..), ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArmInjury = ArmInjury TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armInjury :: TreacheryCard ArmInjury
armInjury = treachery ArmInjury Cards.armInjury

instance HasModifiersFor ArmInjury where
  getModifiersFor target (ArmInjury a) = modified a [IsPointOfDamage | isTarget a target]

instance HasAbilities ArmInjury where
  getAbilities (ArmInjury a) =
    [ restrictedAbility a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf
        $ AbilityIsAction
        <$> [#fight, #activate]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage ArmInjury where
  runMessage msg t@(ArmInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier
        (attrs.ability 1)
        iid
        (CannotTakeAction $ AnyActionTarget $ map IsAction [#fight, #activate])
      pure . ArmInjury $ setMeta False attrs
    EndTurn _ -> do
      pure . ArmInjury $ setMeta True attrs
    _ -> ArmInjury <$> lift (runMessage msg attrs)
