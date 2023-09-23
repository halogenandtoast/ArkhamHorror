module Arkham.Treachery.Cards.Psychosis (
  Psychosis (..),
  psychosis,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Psychosis = Psychosis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychosis :: TreacheryCard Psychosis
psychosis = treachery Psychosis Cards.psychosis

instance HasAbilities Psychosis where
  getAbilities (Psychosis a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ DealtHorror #after AnySource You
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing (ActionCost 2)
    ]

instance RunMessage Psychosis where
  runMessage msg t@(Psychosis attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ directDamage iid (toAbilitySource attrs 1) 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> Psychosis <$> runMessage msg attrs
