module Arkham.Event.Cards.Persuasion (
  persuasion,
  Persuasion (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait

newtype Persuasion = Persuasion EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persuasion :: EventCard Persuasion
persuasion = event Persuasion Cards.persuasion

instance RunMessage Persuasion where
  runMessage msg e@(Persuasion attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      mlocation <- field InvestigatorLocation iid
      case mlocation of
        Just location -> do
          enemies <-
            select
              $ enemyAt location
              <> EnemyWithTrait Humanoid
              <> NonWeaknessEnemy
              <> canParleyEnemy iid
          player <- getPlayer iid
          sid <- getRandom
          pushAll
            [ chooseOne
                player
                [ targetLabel
                  enemy
                  [ parley
                      sid
                      iid
                      (toSource attrs)
                      (EnemyTarget enemy)
                      SkillIntellect
                      (SumCalculation [Fixed 3, EnemyFieldCalculation enemy EnemySanityDamage])
                  ]
                | enemy <- enemies
                ]
            ]
        _ -> error "investigator not at location"
      pure e
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ _ -> do
      mSkillTestTarget <- getSkillTestTarget
      case mSkillTestTarget of
        Just (EnemyTarget eid) -> do
          isElite <- eid <=~> EliteEnemy
          if isElite
            then push $ EnemyEvaded iid eid
            else push $ ShuffleBackIntoEncounterDeck (EnemyTarget eid)
        _ -> error "Invalid target"
      pure e
    _ -> Persuasion <$> runMessage msg attrs
