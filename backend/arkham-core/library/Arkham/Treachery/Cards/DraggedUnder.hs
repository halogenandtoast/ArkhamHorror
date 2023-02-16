module Arkham.Treachery.Cards.DraggedUnder where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DraggedUnder = DraggedUnder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnder :: TreacheryCard DraggedUnder
draggedUnder = treachery DraggedUnder Cards.draggedUnder

instance HasAbilities DraggedUnder where
  getAbilities (DraggedUnder x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility $ Leaves
      Timing.When
      You
      Anywhere
    , restrictedAbility x 2 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
      Timing.When
      You
    ]

instance RunMessage DraggedUnder where
  runMessage msg t@(DraggedUnder attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillAgility 3)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> t <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 2 0
      , Discard (toAbilitySource attrs 1) $ toTarget attrs
      ]
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ beginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        SkillAgility
        3
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ when
        (isNothing $ treacheryAttachedTarget attrs)
        (push $ AttachTreachery (toId attrs) (InvestigatorTarget iid))
    _ -> DraggedUnder <$> runMessage msg attrs
