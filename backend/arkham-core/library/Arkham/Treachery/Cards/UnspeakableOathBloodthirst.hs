module Arkham.Treachery.Cards.UnspeakableOathBloodthirst (
  unspeakableOathBloodthirst,
  UnspeakableOathBloodthirst (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (TreacheryInHandOf)
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UnspeakableOathBloodthirst = UnspeakableOathBloodthirst TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

unspeakableOathBloodthirst :: TreacheryCard UnspeakableOathBloodthirst
unspeakableOathBloodthirst = treachery UnspeakableOathBloodthirst Cards.unspeakableOathBloodthirst

instance HasAbilities UnspeakableOathBloodthirst where
  getAbilities (UnspeakableOathBloodthirst attrs) =
    [ restrictedAbility attrs 1 InYourHand
        $ ForcedAbility
        $ OrWindowMatcher
          [ Matcher.GameEnds #when
          , Matcher.InvestigatorEliminated #when You
          ]
    , restrictedAbility attrs 2 InYourHand
        $ freeReaction
        $ EnemyDealtExcessDamage #when AnyDamageEffect AnyEnemy (SourceOwnedBy You)
    ]

instance RunMessage UnspeakableOathBloodthirst where
  runMessage msg t@(UnspeakableOathBloodthirst attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ gameModifier attrs iid (XPModifier (-2))
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> UnspeakableOathBloodthirst <$> runMessage msg attrs
