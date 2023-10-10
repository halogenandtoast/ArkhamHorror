module Arkham.Act.Cards.Run (
  Run (..),
  run,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner hiding (Run)
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Metadata = Metadata {advancingInvestigator :: Maybe InvestigatorId}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype Run = Run (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

run :: ActCard Run
run = act (1, A) (Run . (`with` (Metadata Nothing))) Cards.run Nothing

instance HasAbilities Run where
  getAbilities (Run x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ Enters Timing.When You
        $ LocationWithTitle
          "Engine Car"
    ]

instance RunMessage Run where
  runMessage msg a@(Run (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (AdvanceAct (toId attrs) source AdvancedWithOther)
      -- We need to know the investigator who entered
      pure $ Run $ attrs `with` Metadata (Just iid)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      case advancingInvestigator metadata of
        Nothing -> error "investigator should have advanced"
        Just iid -> do
          player <- getPlayer iid
          pushAll
            $ chooseOne
              player
              [ Label
                  "Attempt to dodge the creature"
                  [beginSkillTest iid attrs attrs #agility 3]
              , Label
                  "Attempt to endure the creature's extreme heat"
                  [beginSkillTest iid attrs attrs #combat 3]
              ]
            : [advanceActDeck attrs]
      pure a
    FailedSkillTest iid _ source Initiator {} (SkillSkillTest SkillAgility) _ | isSource attrs source && onSide B attrs -> do
      push (SufferTrauma iid 1 0)
      pure a
    FailedSkillTest iid _ source Initiator {} (SkillSkillTest SkillCombat) _ | isSource attrs source && onSide B attrs -> do
      push (SufferTrauma iid 1 0)
      pure a
    _ -> Run . (`with` metadata) <$> runMessage msg attrs
