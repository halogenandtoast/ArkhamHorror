module Arkham.Treachery.Cards.ConspiracyOfBlood
  ( conspiracyOfBlood
  , ConspiracyOfBlood(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ConspiracyOfBlood = ConspiracyOfBlood TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conspiracyOfBlood :: TreacheryCard ConspiracyOfBlood
conspiracyOfBlood = treachery ConspiracyOfBlood Cards.conspiracyOfBlood

instance HasModifiersFor ConspiracyOfBlood where
  getModifiersFor (AgendaTarget aid) (ConspiracyOfBlood a)
    | treacheryOnAgenda aid a = pure
    $ toModifiers a [DoomThresholdModifier (-1)]
  getModifiersFor _ _ = pure []

instance HasAbilities ConspiracyOfBlood where
  getAbilities (ConspiracyOfBlood attrs) =
    [ restrictedAbility
          (ProxySource
            (EnemyMatcherSource $ EnemyWithTrait Cultist)
            (toSource attrs)
          )
          1
          OnSameLocation
        $ ActionAbility (Just Action.Parley)
        $ ActionCost 1
    ]

instance RunMessage ConspiracyOfBlood where
  runMessage msg t@(ConspiracyOfBlood attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      currentAgenda <- selectJust AnyAgenda
      push $ AttachTreachery (toId attrs) (AgendaTarget currentAgenda)
      pure t
    UseCardAbility iid (ProxySource (EnemySource eid) source) 1 _ _
      | isSource attrs source -> do
        push $ parley
          iid
          source
          (EnemyTarget eid)
          SkillWillpower
          4
        pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
        pure t
    FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        mTarget <- getSkillTestTarget
        case mTarget of
          Just (EnemyTarget eid) -> push $ PlaceDoom (EnemyTarget eid) 1
          _ -> pure ()
        pure t
    _ -> ConspiracyOfBlood <$> runMessage msg attrs
