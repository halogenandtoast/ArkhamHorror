module Arkham.Treachery.Cards.Hypochondria (
  Hypochondria (..),
  hypochondria,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Hypochondria = Hypochondria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypochondria :: TreacheryCard Hypochondria
hypochondria = treachery Hypochondria Cards.hypochondria

instance HasAbilities Hypochondria where
  getAbilities (Hypochondria a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ DealtDamage #after AnySource You
    , restrictedAbility a 2 OnSameLocation $ ActionAbility [] (ActionCost 2)
    ]

instance RunMessage Hypochondria where
  runMessage msg t@(Hypochondria attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ directHorror iid (toAbilitySource attrs 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> Hypochondria <$> runMessage msg attrs
