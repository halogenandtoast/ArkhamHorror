module Arkham.Types.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Exception
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.ScenarioId
import Arkham.Types.Target

newtype RicesWhereabouts = RicesWhereabouts ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

ricesWhereabouts :: ActCard RicesWhereabouts
ricesWhereabouts = act (2, A) RicesWhereabouts Cards.ricesWhereabouts Nothing

instance HasActions RicesWhereabouts where
  getActions (RicesWhereabouts x) =
    mkAbility x 1 (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
      : getActions x

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs@ActAttrs {..}) = case msg of
    TakeControlOfAsset _ assetId -> do
      cardCode <- getId assetId
      a <$ when
        (cardCode == CardCode "02060")
        (push $ AdvanceAct actId (toSource attrs))
    Discarded (InvestigatorTarget iid) card
      | toCardCode card == CardCode "02060" -> case card of
        EncounterCard ec ->
          a
            <$ pushAll
                 [ RemoveFromEncounterDiscard ec
                 , InvestigatorDrewEncounterCard iid ec
                 ]
        PlayerCard _ -> throwIO $ InvalidState "not a player card"
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      playerCount <- getPlayerCount
      let discardCount = if playerCount == 1 then 10 else 5
      a <$ push (DiscardTopOfEncounterDeck iid discardCount Nothing)
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      push (AdvanceAct aid $ toSource attrs)
      pure . RicesWhereabouts $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      alchemyLabsInPlay <- isJust <$> getLocationIdWithTitle "Alchemy Labs"
      agendaStep <- unAgendaStep <$> getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()
      theExperiment <- EncounterCard <$> genEncounterCard Enemies.theExperiment
      alchemicalConcoction <- EncounterCard
        <$> genEncounterCard Assets.alchemicalConcoction

      pushAll
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
      push $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ RicesWhereabouts $ attrs & (sequenceL .~ Act 1 B)
    _ -> RicesWhereabouts <$> runMessage msg attrs
