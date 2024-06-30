module Arkham.Treachery.Cards.LegInjury (legInjury, LegInjury (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ActionTarget (..), ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LegInjury = LegInjury TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

legInjury :: TreacheryCard LegInjury
legInjury = treachery LegInjury Cards.legInjury

instance HasModifiersFor LegInjury where
  getModifiersFor target (LegInjury a) = modified a [IsPointOfDamage | isTarget a target]

instance HasAbilities LegInjury where
  getAbilities (LegInjury a) =
    [ restrictedAbility a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf
        $ AbilityIsAction
        <$> [#move, #resign, #evade]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage LegInjury where
  runMessage msg t@(LegInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier
        (attrs.ability 1)
        iid
        (CannotTakeAction $ AnyActionTarget $ map IsAction [#move, #resign, #evade])
      pure . LegInjury $ setMeta False attrs
    EndTurn _ -> do
      pure . LegInjury $ setMeta True attrs
    _ -> LegInjury <$> lift (runMessage msg attrs)
