module Arkham.Treachery.Cards.LegInjury (legInjury, LegInjury (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LegInjury = LegInjury TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

legInjury :: TreacheryCard LegInjury
legInjury = treachery LegInjury Cards.legInjury

instance HasModifiersFor LegInjury where
  getModifiersFor (LegInjury a) = modifySelf a [IsPointOfDamage]

instance HasAbilities LegInjury where
  getAbilities (LegInjury a) =
    [ restrictedAbility a 1 injuryCriteria
        $ forced
        $ ActivateAbility #after You
        $ oneOf [#move, #resign, #evade]
    ]
   where
    injuryCriteria = if toResultDefault True a.meta then InThreatAreaOf You else Never

instance RunMessage LegInjury where
  runMessage msg t@(LegInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne TurnInvestigator >>= \case
        Nothing -> pure t
        Just iid' -> do
          turnModifier iid' (attrs.ability 1) iid
            $ CannotTakeAction
            $ AnyActionTarget [#move, #resign, #evade]
          pure . LegInjury $ setMeta False attrs
    EndTurn _ -> pure . LegInjury $ setMeta True attrs
    _ -> LegInjury <$> liftRunMessage msg attrs
