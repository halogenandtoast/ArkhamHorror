module Arkham.Types.Act.Cards.Trapped where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype Trapped = Trapped ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapped :: Trapped
trapped = Trapped $ baseAttrs
  "01108"
  "Trapped"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 2) Nothing)

instance ActionRunner env => HasActions env Trapped where
  getActions i window (Trapped x) = getActions i window x

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs]
        ]
      pure $ Trapped $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      enemyIds <- getSetList (LocationId "01111")
      a <$ unshiftMessages
        ([ PlaceLocation "01112"
         , PlaceLocation "01114"
         , PlaceLocation "01113"
         , PlaceLocation "01115"
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation Nothing "01112"
           , MoveAllTo "01112"
           , RemoveLocation "01111"
           , NextAct aid "01109"
           ]
        )
    _ -> Trapped <$> runMessage msg attrs
