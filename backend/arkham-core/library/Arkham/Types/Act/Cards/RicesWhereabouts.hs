module Arkham.Types.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.ScenarioId
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype RicesWhereabouts = RicesWhereabouts ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ricesWhereabouts :: RicesWhereabouts
ricesWhereabouts =
  RicesWhereabouts $ baseAttrs "02046" "Rice's Whereabouts" (Act 2 A) Nothing

ability :: ActAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])

instance ActionRunner env => HasActions env RicesWhereabouts where
  getActions iid NonFast (RicesWhereabouts x) =
    pure [ActivateCardAbilityAction iid (ability x)]
  getActions iid window (RicesWhereabouts x) = getActions iid window x

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs@ActAttrs {..}) = case msg of
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
      theExperiment <- EncounterCard <$> genEncounterCard "02058"
      alchemicalConcoction <- PlayerCard <$> genPlayerCard "02059"

      unshiftMessages
        $ [ PlaceLocationMatching (LocationWithTitle "Alchemy Labs")
          | not alchemyLabsInPlay
          ]
        <> [ CreateEnemyAtLocationMatching
               theExperiment
               (LocationWithTitle "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               alchemicalConcoction
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ RicesWhereabouts $ attrs & (sequenceL .~ Act 1 B)
    _ -> RicesWhereabouts <$> runMessage msg attrs
