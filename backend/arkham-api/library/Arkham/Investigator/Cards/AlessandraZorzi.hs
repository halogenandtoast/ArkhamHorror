module Arkham.Investigator.Cards.AlessandraZorzi (alessandraZorzi, AlessandraZorzi (..)) where

import Arkham.Action.Additional
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype AlessandraZorzi = AlessandraZorzi InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

alessandraZorzi :: InvestigatorCard AlessandraZorzi
alessandraZorzi =
  investigator AlessandraZorzi Cards.alessandraZorzi
    $ Stats {health = 7, sanity = 7, willpower = 3, intellect = 4, combat = 2, agility = 4}

instance HasModifiersFor AlessandraZorzi where
  getModifiersFor target (AlessandraZorzi a) | a `is` target = do
    modified
      a
      [ GiveAdditionalAction
          $ AdditionalAction "Alessadra Zorzi" (toSource a)
          $ ActionRestrictedAdditionalAction #parley
      ]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue AlessandraZorzi where
  getChaosTokenValue iid ElderSign (AlessandraZorzi attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AlessandraZorzi where
  runMessage msg i@(AlessandraZorzi attrs) = runQueueT $ case msg of
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      selectOneToHandle iid iid
        $ NonEliteEnemy
        <> EnemyAt
          ( oneOf [locationWithInvestigator iid, RevealedLocation <> ConnectedTo (locationWithInvestigator iid)]
          )
      pure i
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      automaticallyEvadeEnemy iid eid
      pure i
    _ -> AlessandraZorzi <$> liftRunMessage msg attrs
