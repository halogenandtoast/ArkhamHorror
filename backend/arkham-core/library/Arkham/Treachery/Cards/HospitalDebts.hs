module Arkham.Treachery.Cards.HospitalDebts (
  HospitalDebts (..),
  hospitalDebts,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype HospitalDebts = HospitalDebts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

hospitalDebts :: TreacheryCard HospitalDebts
hospitalDebts = treachery HospitalDebts Cards.hospitalDebts

instance HasAbilities HospitalDebts where
  getAbilities (HospitalDebts a) =
    limitedAbility
      (PlayerLimit PerRound 2)
      ( restrictedAbility a 1 (OnSameLocation <> exists (You <> InvestigatorWithResources (atLeast 1)))
          $ FastAbility Free
      )
      : [ restrictedAbility a 2 (ResourcesOnThis $ lessThan 6) $ forcedOnElimination iid
        | iid <- toList a.owner
        ]

instance RunMessage HospitalDebts where
  runMessage msg t@(HospitalDebts attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [SpendResources iid 1, PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ gameModifier (toAbilitySource attrs 2) iid (XPModifier (-2))
      pure t
    _ -> HospitalDebts <$> runMessage msg attrs
