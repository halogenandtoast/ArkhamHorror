module Arkham.Treachery.Cards.DraggedUnder where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DraggedUnder = DraggedUnder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

draggedUnder :: TreacheryCard DraggedUnder
draggedUnder = treachery DraggedUnder Cards.draggedUnder

instance HasAbilities DraggedUnder where
  getAbilities (DraggedUnder x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ Leaves #when You Anywhere
    , restrictedAbility x 2 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    ]

instance RunMessage DraggedUnder where
  runMessage msg t@(DraggedUnder attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RevelationSkillTest iid (toSource attrs) #agility 3
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ assignDamage iid source 2
        , toDiscardBy iid source attrs
        ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) iid #agility 3
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushWhen
        (isNothing $ treacheryAttachedTarget attrs)
        (attachTreachery attrs iid)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> DraggedUnder <$> runMessage msg attrs
