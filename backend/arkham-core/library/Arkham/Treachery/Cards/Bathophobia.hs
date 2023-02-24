module Arkham.Treachery.Cards.Bathophobia
  ( bathophobia
  , Bathophobia(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Classes
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Bathophobia = Bathophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bathophobia :: TreacheryCard Bathophobia
bathophobia = treachery Bathophobia Cards.bathophobia

instance RunMessage Bathophobia where
  runMessage msg t@(Bathophobia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      n <- getCurrentDepth
      push $ RevelationSkillTest iid source SkillWillpower (1 + n)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
        pure t
    _ -> Bathophobia <$> runMessage msg attrs
