module Arkham.Treachery.Cards.ArmInjury (armInjury, ArmInjury (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArmInjury = ArmInjury TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armInjury :: TreacheryCard ArmInjury
armInjury = treachery ArmInjury Cards.armInjury

instance HasModifiersFor ArmInjury where
  getModifiersFor (ArmInjury a) = modifySelf a [IsPointOfDamage]

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
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne TurnInvestigator >>= \case
        Nothing -> pure t
        Just iid' -> do
          turnModifier
            iid'
            (attrs.ability 1)
            iid
            (CannotTakeAction $ AnyActionTarget $ map IsAction [#fight, #activate])
          pure . ArmInjury $ setMeta False attrs
    EndTurn _ -> do
      pure . ArmInjury $ setMeta True attrs
    _ -> ArmInjury <$> liftRunMessage msg attrs
