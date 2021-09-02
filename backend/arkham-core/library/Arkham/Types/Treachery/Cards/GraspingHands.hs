module Arkham.Types.Treachery.Cards.GraspingHands
  ( GraspingHands(..)
  , graspingHands
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype GraspingHands = GraspingHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingHands :: TreacheryCard GraspingHands
graspingHands = treachery GraspingHands Cards.graspingHands

instance TreacheryRunner env => RunMessage env GraspingHands where
  runMessage msg t@(GraspingHands attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [RevelationSkillTest iid source SkillAgility 3, Discard $ toTarget attrs]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t
      <$ push (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> GraspingHands <$> runMessage msg attrs
