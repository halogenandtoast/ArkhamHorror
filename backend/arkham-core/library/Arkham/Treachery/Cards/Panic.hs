module Arkham.Treachery.Cards.Panic (panic, Panic (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ActionTarget (..), ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Panic = Panic TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

panic :: TreacheryCard Panic
panic = treachery Panic Cards.panic

instance HasModifiersFor Panic where
  getModifiersFor target (Panic a) = modified a [IsPointOfHorror | isTarget a target]

instance HasAbilities Panic where
  getAbilities (Panic a) =
    [ restrictedAbility a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf
        $ AbilityIsAction
        <$> [#play, #engage, #resource]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage Panic where
  runMessage msg t@(Panic attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier
        (attrs.ability 1)
        iid
        (CannotTakeAction $ AnyActionTarget $ map IsAction [#play, #engage, #resource])
      pure . Panic $ setMeta False attrs
    EndTurn _ -> do
      pure . Panic $ setMeta True attrs
    _ -> Panic <$> lift (runMessage msg attrs)
