module Arkham.Treachery.Cards.HospitalDebtsAdvanced (HospitalDebtsAdvanced (..), hospitalDebtsAdvanced) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HospitalDebtsAdvanced = HospitalDebtsAdvanced TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hospitalDebtsAdvanced :: TreacheryCard HospitalDebtsAdvanced
hospitalDebtsAdvanced = treachery HospitalDebtsAdvanced Cards.hospitalDebtsAdvanced

instance HasAbilities HospitalDebtsAdvanced where
  getAbilities (HospitalDebtsAdvanced a) =
    limitedAbility
      (PlayerLimit PerRound 3)
      ( restrictedAbility a 1 (OnSameLocation <> youExist (InvestigatorWithResources $ atLeast 1))
          $ FastAbility Free
      )
      : [ restrictedAbility a 2 (ResourcesOnThis $ lessThan 9) $ forcedOnElimination iid
        | iid <- toList a.inThreatAreaOf
        ]

instance RunMessage HospitalDebtsAdvanced where
  runMessage msg t@(HospitalDebtsAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SpendResources iid 1, PlaceResources (attrs.ability 1) (toTarget attrs) 1]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gameModifier (attrs.ability 2) iid (XPModifier (-2))
      pure t
    _ -> HospitalDebtsAdvanced <$> liftRunMessage msg attrs
