module Arkham.Types.Treachery.Cards.CalledByTheMists
  ( calledByTheMists
  , CalledByTheMists(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CalledByTheMists = CalledByTheMists TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calledByTheMists :: TreacheryCard CalledByTheMists
calledByTheMists = treachery CalledByTheMists Cards.calledByTheMists

instance HasAbilities CalledByTheMists where
  getAbilities (CalledByTheMists a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
      $ ForcedAbility
      $ InitiatedSkillTest Timing.After You AnySkillTest (AtLeast $ Static 3)
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing $ ActionCost
      2
    ]

instance TreacheryRunner env => RunMessage env CalledByTheMists where
  runMessage msg t@(CalledByTheMists attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> CalledByTheMists <$> runMessage msg attrs
