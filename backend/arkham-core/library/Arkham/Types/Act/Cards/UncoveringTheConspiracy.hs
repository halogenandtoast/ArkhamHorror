module Arkham.Types.Act.Cards.UncoveringTheConspiracy
  ( UncoveringTheConspiracy(..)
  , uncoveringTheConspiracy
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import qualified Data.HashSet as HashSet

newtype UncoveringTheConspiracy = UncoveringTheConspiracy ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncoveringTheConspiracy :: UncoveringTheConspiracy
uncoveringTheConspiracy = UncoveringTheConspiracy
  $ baseAttrs "01123" "Uncovering the Conspiracy" (Act 1 A) Nothing

instance ActionRunner env => HasActions env UncoveringTheConspiracy where
  getActions iid NonFast (UncoveringTheConspiracy x@ActAttrs {..}) = do
    requiredClues <- getPlayerCountValue (PerPlayer 2)
    totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
    if totalSpendableClues >= requiredClues
      then pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (ActSource actId)
              1
              (ActionAbility Nothing $ ActionCost 1)
            )
        ]
      else getActions iid NonFast x
  getActions iid window (UncoveringTheConspiracy attrs) =
    getActions iid window attrs

instance ActRunner env => RunMessage env UncoveringTheConspiracy where
  runMessage msg a@(UncoveringTheConspiracy attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs])
      pure $ UncoveringTheConspiracy $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    AddToVictory _ -> do
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
      let
        cultists =
          setFromList ["01121b", "01137", "01138", "01139", "01140", "01141"]
      a <$ when
        (cultists `HashSet.isSubsetOf` victoryDisplay)
        (unshiftMessage (AdvanceAct actId $ toSource attrs))
    UseCardAbility iid (ActSource aid) _ 1 _ | aid == actId -> do
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      a <$ unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , UseScenarioSpecificAbility iid 1
        ]
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs
