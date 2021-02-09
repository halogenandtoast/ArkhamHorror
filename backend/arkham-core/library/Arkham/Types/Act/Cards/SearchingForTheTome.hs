module Arkham.Types.Act.Cards.SearchingForTheTome
  ( SearchingForTheTome(..)
  , searchingForTheTome
  )
where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype SearchingForTheTome = SearchingForTheTome ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForTheTome :: SearchingForTheTome
searchingForTheTome = SearchingForTheTome
  $ baseAttrs "02125" "Searching for the Tome" (Act 3 A) Nothing

instance ActionRunner env => HasActions env SearchingForTheTome where
  getActions i window (SearchingForTheTome x) = do
    mRestrictedHall <- getId @(Maybe LocationId)
      (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
    case mRestrictedHall of
      Just restrictedHall -> do
        mustAdvance <- (== 0) . unClueCount <$> getCount restrictedHall
        if mustAdvance
          then pure [Force $ AdvanceAct (actId x) (toSource x)]
          else getActions i window x
      Nothing -> getActions i window x

instance ActRunner env => RunMessage env SearchingForTheTome where
  runMessage msg a@(SearchingForTheTome attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure . SearchingForTheTome $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Label
            "It's too dangerous to keep around. We have to destroy it. (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "It's too valuable to destroy. We have to keep it safe. (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
    _ -> SearchingForTheTome <$> runMessage msg attrs
