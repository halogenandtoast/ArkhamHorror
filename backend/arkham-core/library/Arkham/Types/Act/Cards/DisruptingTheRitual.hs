module Arkham.Types.Act.Cards.DisruptingTheRitual where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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
  getAbilities _ (Window Timing.When NonFast) (DisruptingTheRitual a) = pure
    [ mkAbility
        (toSource a)
        1
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]
  getAbilities i window (DisruptingTheRitual x) = getAbilities i window x

instance ActRunner env => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push (chooseOne leadInvestigatorId [AdvanceAct actId (toSource attrs)])
      pure $ DisruptingTheRitual $ attrs & (sequenceL .~ Act 3 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    PlaceClues (ActTarget aid) n | aid == actId -> do
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      let totalClues = n + fromJustNote "Must be set" actClues
      when
        (totalClues >= requiredClues)
        (push (AdvanceAct actId $ toSource attrs))
      pure $ DisruptingTheRitual (attrs { actClues = Just totalClues })
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId -> a <$ push
      (chooseOne
        iid
        [ BeginSkillTest
          iid
          (ActSource actId)
          (ActTarget actId)
          Nothing
          SkillWillpower
          3
        , BeginSkillTest
          iid
          (ActSource actId)
          (ActTarget actId)
          Nothing
          SkillAgility
          3
        ]
      )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (PlaceClues (toTarget attrs) 1)
    _ -> DisruptingTheRitual <$> runMessage msg attrs
