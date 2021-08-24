module Arkham.Types.Act.Cards.DisruptingTheRitual where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: ActCard DisruptingTheRitual
disruptingTheRitual = actWith
  (3, A)
  DisruptingTheRitual
  Cards.disruptingTheRitual
  Nothing
  (cluesL ?~ 0)

instance HasAbilities env DisruptingTheRitual where
  getAbilities _ _ (DisruptingTheRitual a) | onSide A a = pure
    [ mkAbility a 1 $ ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]
    , restrictedAbility a 2 (CluesOnThis $ AtLeast $ PerPlayer 2)
    $ Objective
    $ ForcedAbility AnyWindow
    ]
  getAbilities _ _ _ = pure []

instance ActRunner env => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 3
        , BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 3
        ]
      )
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) source)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (PlaceClues (toTarget attrs) 1)
    _ -> DisruptingTheRitual <$> runMessage msg attrs
