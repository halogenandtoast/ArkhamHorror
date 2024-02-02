module Arkham.Treachery.Cards.InternalInjury (
  internalInjury,
  InternalInjury (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype InternalInjury = InternalInjury TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

internalInjury :: TreacheryCard InternalInjury
internalInjury = treachery InternalInjury Cards.internalInjury

instance HasAbilities InternalInjury where
  getAbilities (InternalInjury x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    , restrictedAbility x 2 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage InternalInjury where
  runMessage msg t@(InternalInjury attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ directDamage iid (toAbilitySource attrs 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> InternalInjury <$> runMessage msg attrs
