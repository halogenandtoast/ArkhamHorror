module Arkham.Treachery.Cards.Narcolepsy (
  narcolepsy,
  Narcolepsy (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Narcolepsy = Narcolepsy TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narcolepsy :: TreacheryCard Narcolepsy
narcolepsy = treachery Narcolepsy Cards.narcolepsy

instance HasModifiersFor Narcolepsy where
  getModifiersFor (InvestigatorTarget iid) (Narcolepsy attrs) | treacheryOnInvestigator iid attrs = do
    pure
      $ toModifiers
        attrs
        [CannotTakeAction IsAnyAction, CannotTriggerAbilityMatching AnyAbility, CannotPlay AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities Narcolepsy where
  getAbilities (Narcolepsy a) =
    [ restrictedAbility a 1 OnSameLocation actionAbility
    , restrictedAbility a 2 ControlsThis
        $ ForcedAbility
        $ oneOf [InvestigatorTakeDamage #after You AnySource, InvestigatorTakeHorror #after You AnySource]
    ]

instance RunMessage Narcolepsy where
  runMessage msg t@(Narcolepsy attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> Narcolepsy <$> runMessage msg attrs
