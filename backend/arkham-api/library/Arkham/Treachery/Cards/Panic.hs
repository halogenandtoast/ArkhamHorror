module Arkham.Treachery.Cards.Panic (panic, Panic (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Panic = Panic TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

panic :: TreacheryCard Panic
panic = treachery Panic Cards.panic

instance HasModifiersFor Panic where
  getModifiersFor (Panic a) = modifySelf a [IsPointOfHorror]

instance HasAbilities Panic where
  getAbilities (Panic a) =
    [ restricted a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf [#play, #engage, #resource]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage Panic where
  runMessage msg t@(Panic attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne TurnInvestigator >>= \case
        Nothing -> pure t
        Just iid' -> do
          turnModifier iid' (attrs.ability 1) iid
            $ CannotTakeAction
            $ AnyActionTarget [#play, #engage, #resource]
          pure . Panic $ setMeta False attrs
    EndTurn _ -> pure . Panic $ setMeta True attrs
    _ -> Panic <$> liftRunMessage msg attrs
