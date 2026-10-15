module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.AuditoryHallucinations (
  auditoryHallucinations,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype AuditoryHallucinations = AuditoryHallucinations TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

auditoryHallucinations :: TreacheryCard AuditoryHallucinations
auditoryHallucinations = treachery AuditoryHallucinations Cards.auditoryHallucinations

instance HasModifiersFor AuditoryHallucinations where
  -- "Each investigator's maximum hand size is reduced by 2."
  getModifiersFor (AuditoryHallucinations a) =
    modifySelectWhen a (a.placement == NextToAgenda) Anyone [HandSize (-2)]

instance HasAbilities AuditoryHallucinations where
  getAbilities (AuditoryHallucinations a) =
    [mkAbility a 1 $ actionAbilityWithCost (PlaceClueOnLocationCost (Static 1))]

instance RunMessage AuditoryHallucinations where
  runMessage msg t@(AuditoryHallucinations attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> AuditoryHallucinations <$> liftRunMessage msg attrs
