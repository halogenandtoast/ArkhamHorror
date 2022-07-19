module Arkham.Treachery.Cards.Hypochondria
  ( Hypochondria(..)
  , hypochondria
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner

newtype Hypochondria = Hypochondria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypochondria :: TreacheryCard Hypochondria
hypochondria = treachery Hypochondria Cards.hypochondria

instance HasAbilities Hypochondria where
  getAbilities (Hypochondria a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ DealtDamage
      Timing.After
      AnySource
      You
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing $ ActionCost
      2
    ]

instance RunMessage Hypochondria where
  runMessage msg t@(Hypochondria attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (InvestigatorDirectDamage iid source 0 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> Hypochondria <$> runMessage msg attrs
