module Arkham.Treachery.Cards.Psychosis
  ( Psychosis(..)
  , psychosis
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

newtype Psychosis = Psychosis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychosis :: TreacheryCard Psychosis
psychosis = treachery Psychosis Cards.psychosis

instance HasAbilities Psychosis where
  getAbilities (Psychosis a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ DealtHorror
      Timing.After
      AnySource
      You
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing $ ActionCost
      2
    ]

instance RunMessage Psychosis where
  runMessage msg t@(Psychosis attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (InvestigatorDirectDamage iid source 1 0)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> Psychosis <$> runMessage msg attrs
