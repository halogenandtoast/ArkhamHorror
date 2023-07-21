module Arkham.Treachery.Cards.Hypochondria (
  Hypochondria (..),
  hypochondria,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Hypochondria = Hypochondria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypochondria :: TreacheryCard Hypochondria
hypochondria = treachery Hypochondria Cards.hypochondria

instance HasAbilities Hypochondria where
  getAbilities (Hypochondria a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $
        ForcedAbility $
          DealtDamage
            Timing.After
            AnySource
            You
    , restrictedAbility a 2 OnSameLocation $
        ActionAbility Nothing $
          ActionCost
            2
    ]

instance RunMessage Hypochondria where
  runMessage msg t@(Hypochondria attrs) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          t <$ push (InvestigatorDirectDamage iid source 0 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> Hypochondria <$> runMessage msg attrs
