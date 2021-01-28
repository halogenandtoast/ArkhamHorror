module Arkham.Types.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  ) where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype RicesWhereabouts = RicesWhereabouts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ricesWhereabouts :: RicesWhereabouts
ricesWhereabouts =
  RicesWhereabouts $ baseAttrs "02046" "Rice's Whereabouts" (Act 2 A) Nothing

ability :: Attrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])

instance ActionRunner env => HasActions env RicesWhereabouts where
  getActions iid NonFast (RicesWhereabouts x) =
    pure [ActivateCardAbilityAction iid (ability x)]
  getActions iid window (RicesWhereabouts x) = getActions iid window x

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs@Attrs {..}) = case msg of
    TakeControlOfAsset _ assetId -> do
      cardCode <- getId assetId
      a <$ when
        (cardCode == CardCode "02060")
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    Discarded (InvestigatorTarget iid) card
      | getCardCode card == CardCode "02060" -> case card of
        EncounterCard ec -> a <$ unshiftMessages
          [RemoveFromEncounterDiscard ec, InvestigatorDrewEncounterCard iid ec]
        PlayerCard _ -> throwIO $ InvalidState "not a player card"
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      playerCount <- getPlayerCount
      let discardCount = if playerCount == 1 then 10 else 5
      a <$ unshiftMessage (DiscardTopOfEncounterDeck iid discardCount Nothing)
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure . RicesWhereabouts $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      alchemyLabsInPlay <- isJust <$> getLocationIdWithTitle "Alchemy Labs"
      agendaStep <- asks $ unAgendaStep . getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      unshiftMessages
        $ [ PlaceLocationMatching (LocationWithTitle "Alchemy Labs")
          | not alchemyLabsInPlay
          ]
        <> [ CreateEnemyAtLocationMatching
               "02058"
               (LocationWithTitle "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               "02059"
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ RicesWhereabouts $ attrs & (sequenceL .~ Act 1 B)
    _ -> RicesWhereabouts <$> runMessage msg attrs
