module Arkham.Types.Treachery.Cards.Atychiphobia
  ( atychiphobia
  , Atychiphobia(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Atychiphobia = Atychiphobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atychiphobia :: TreacheryCard Atychiphobia
atychiphobia = treachery Atychiphobia Cards.atychiphobia

instance HasAbilities env Atychiphobia where
  getAbilities _ _ (Atychiphobia a) = pure
    [ restrictedAbility a 1 (InThreatAreaOf You)
    $ ForcedAbility
    $ SkillTestResult Timing.After You AnySkillTest
    $ FailureResult AnyValue
    , restrictedAbility a 2 (InThreatAreaOf $ InvestigatorAt YourLocation)
    $ ActionAbility Nothing
    $ ActionCost 2
    ]

instance TreacheryRunner env => RunMessage env Atychiphobia where
  runMessage msg t@(Atychiphobia attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> Atychiphobia <$> runMessage msg attrs
