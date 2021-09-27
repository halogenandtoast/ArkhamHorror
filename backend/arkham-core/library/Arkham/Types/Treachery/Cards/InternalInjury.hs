module Arkham.Types.Treachery.Cards.InternalInjury
  ( internalInjury
  , InternalInjury(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype InternalInjury = InternalInjury TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

internalInjury :: TreacheryCard InternalInjury
internalInjury = treachery InternalInjury Cards.internalInjury

instance HasAbilities InternalInjury where
  getAbilities (InternalInjury x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
      Timing.When
      You
    , restrictedAbility x 2 OnSameLocation $ ActionAbility Nothing $ ActionCost
      2
    ]

instance TreacheryRunner env => RunMessage env InternalInjury where
  runMessage msg t@(InternalInjury attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (InvestigatorDirectDamage iid source 1 0)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> InternalInjury <$> runMessage msg attrs
