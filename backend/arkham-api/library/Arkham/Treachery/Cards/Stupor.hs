module Arkham.Treachery.Cards.Stupor (stupor, Stupor (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Stupor = Stupor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stupor :: TreacheryCard Stupor
stupor = treachery Stupor Cards.stupor

instance HasModifiersFor Stupor where
  getModifiersFor (Stupor a) = modifySelf a [IsPointOfHorror]

instance HasAbilities Stupor where
  getAbilities (Stupor a) =
    [ restricted a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf [#parley, #draw, #investigate]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage Stupor where
  runMessage msg t@(Stupor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne TurnInvestigator >>= \case
        Nothing -> pure t
        Just iid' -> do
          turnModifier iid' (attrs.ability 1) iid
            $ CannotTakeAction
            $ AnyActionTarget [#parley, #draw, #investigate]
          pure . Stupor $ setMeta False attrs
    EndTurn _ -> pure . Stupor $ setMeta True attrs
    _ -> Stupor <$> liftRunMessage msg attrs
