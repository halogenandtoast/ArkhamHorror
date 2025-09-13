module Arkham.Treachery.Cards.Psychosis (psychosis) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Psychosis = Psychosis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychosis :: TreacheryCard Psychosis
psychosis = treachery Psychosis Cards.psychosis

instance HasAbilities Psychosis where
  getAbilities (Psychosis a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ InvestigatorTakeHorror #after You AnySource
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage Psychosis where
  runMessage msg t@(Psychosis attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Psychosis <$> liftRunMessage msg attrs
