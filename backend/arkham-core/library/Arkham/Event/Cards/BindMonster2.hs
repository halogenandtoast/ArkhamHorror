module Arkham.Event.Cards.BindMonster2 (
  bindMonster2,
  BindMonster2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards (bindMonster2)
import Arkham.Event.Runner
import Arkham.Exception
import Arkham.Matcher

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

bindMonster2 :: EventCard BindMonster2
bindMonster2 = event BindMonster2 Cards.bindMonster2

instance HasAbilities BindMonster2 where
  getAbilities (BindMonster2 x) = case eventAttachedTarget x of
    Just (EnemyTarget eid) ->
      [ restrictedAbility x 1 ControlsThis
          $ freeReaction (EnemyWouldReady #when $ EnemyWithId eid)
      ]
    _ -> []

instance RunMessage BindMonster2 where
  runMessage msg e@(BindMonster2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CreateEffect "02031" Nothing (toSource attrs) SkillTestTarget
        , chooseEvadeEnemy iid eid #willpower
        ]
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 ->
      case eventAttachedTarget attrs of
        Just target -> do
          push $ beginSkillTest iid (toAbilitySource attrs 1) target #willpower 3
          pure e
        Nothing -> throwIO $ InvalidState "must be attached"
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      case eventAttachedTarget attrs of
        Just target@(EnemyTarget _) -> withQueue_ (filter (/= Ready target))
        _ -> error "invalid target"
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> BindMonster2 <$> runMessage msg attrs
