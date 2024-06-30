module Arkham.Treachery.Cards.Stupor (stupor, Stupor (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ActionTarget (..), ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Stupor = Stupor TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stupor :: TreacheryCard Stupor
stupor = treachery Stupor Cards.stupor

instance HasModifiersFor Stupor where
  getModifiersFor target (Stupor a) = modified a [IsPointOfHorror | isTarget a target]

instance HasAbilities Stupor where
  getAbilities (Stupor a) =
    [ restrictedAbility a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf
        $ AbilityIsAction
        <$> [#parley, #draw, #investigate]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage Stupor where
  runMessage msg t@(Stupor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier
        (attrs.ability 1)
        iid
        (CannotTakeAction $ AnyActionTarget $ map IsAction [#parley, #draw, #investigate])
      pure . Stupor $ setMeta False attrs
    EndTurn _ -> do
      pure . Stupor $ setMeta True attrs
    _ -> Stupor <$> lift (runMessage msg attrs)
