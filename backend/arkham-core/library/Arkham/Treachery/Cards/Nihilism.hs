module Arkham.Treachery.Cards.Nihilism (
  nihilism,
  Nihilism (..),
)
where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (IgnoreChaosToken)

newtype Nihilism = Nihilism TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

nihilism :: TreacheryCard Nihilism
nihilism = treachery Nihilism Cards.nihilism

instance HasAbilities Nihilism where
  getAbilities (Nihilism a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ oneOf
          [ RevealChaosToken #after You #autofail
          , CancelChaosToken #after You #autofail
          , IgnoreChaosToken #after You #autofail
          ]
    , restrictedAbility a 2 OnSameLocation (ActionAbility [] $ ActionCost 2)
    ]

instance RunMessage Nihilism where
  runMessage msg t@(Nihilism attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignDamageAndHorror iid (toAbilitySource attrs 1) 1 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> Nihilism <$> runMessage msg attrs
