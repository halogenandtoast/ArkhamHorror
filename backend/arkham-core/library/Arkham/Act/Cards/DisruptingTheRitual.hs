module Arkham.Act.Cards.DisruptingTheRitual where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.SkillType
import Arkham.Target

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: ActCard DisruptingTheRitual
disruptingTheRitual = act
  (3, A)
  DisruptingTheRitual
  Cards.disruptingTheRitual
  Nothing

instance HasAbilities DisruptingTheRitual where
  getAbilities (DisruptingTheRitual a) | onSide A a =
    [ mkAbility a 1 $ ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]
    , restrictedAbility a 2 (CluesOnThis $ AtLeast $ PerPlayer 2)
      $ Objective
      $ ForcedAbility AnyWindow
    ]
  getAbilities _ = []

instance RunMessage DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ SkillLabel SkillWillpower [BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 3]
        , SkillLabel SkillAgility [BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 3]
        ]
      )
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (PlaceClues (toTarget attrs) 1)
    _ -> DisruptingTheRitual <$> runMessage msg attrs
