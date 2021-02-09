module Arkham.Types.Act.Cards.DisruptingTheRitual where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: DisruptingTheRitual
disruptingTheRitual =
  DisruptingTheRitual
    $ (baseAttrs "01148" "Disrupting the Ritual" (Act 3 A) Nothing)
        { actClues = Just 0
        }

instance ActionRunner env => HasActions env DisruptingTheRitual where
  getActions iid NonFast (DisruptingTheRitual a@ActAttrs {..}) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
        )
    ]
  getActions i window (DisruptingTheRitual x) = getActions i window x

instance ActRunner env => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct actId (toSource attrs)])
      pure $ DisruptingTheRitual $ attrs & (sequenceL .~ Act 3 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    PlaceClues (ActTarget aid) n | aid == actId -> do
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      let totalClues = n + fromJustNote "Must be set" actClues
      when
        (totalClues >= requiredClues)
        (unshiftMessage (AdvanceAct actId $ toSource attrs))
      pure $ DisruptingTheRitual (attrs { actClues = Just totalClues })
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId ->
      a <$ unshiftMessage
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
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      a <$ unshiftMessage (PlaceClues (toTarget attrs) 1)
    _ -> DisruptingTheRitual <$> runMessage msg attrs
