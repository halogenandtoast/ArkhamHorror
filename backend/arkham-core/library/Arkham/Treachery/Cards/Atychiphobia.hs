module Arkham.Treachery.Cards.Atychiphobia
  ( atychiphobia
  , Atychiphobia(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner

newtype Atychiphobia = Atychiphobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atychiphobia :: TreacheryCard Atychiphobia
atychiphobia = treachery Atychiphobia Cards.atychiphobia

instance HasAbilities Atychiphobia where
  getAbilities (Atychiphobia a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
    $ ForcedAbility
    $ SkillTestResult Timing.After You AnySkillTest
    $ FailureResult AnyValue
    , restrictedAbility a 2 (InThreatAreaOf $ InvestigatorAt YourLocation)
    $ ActionAbility Nothing
    $ ActionCost 2
    ]

instance RunMessage Atychiphobia where
  runMessage msg t@(Atychiphobia attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 2) $ toTarget attrs)
    _ -> Atychiphobia <$> runMessage msg attrs
