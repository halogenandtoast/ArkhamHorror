module Arkham.Types.Act.Cards.BeginnersLuck
  ( BeginnersLuck(..)
  , beginnersLuck
  ) where

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
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beginnersLuck :: BeginnersLuck
beginnersLuck =
  BeginnersLuck $ baseAttrs "02066" "Beginner's Luck" (Act 1 A) Nothing

ability :: ActAttrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ReactionAbility Free))
  { abilityLimit = GroupLimit PerRound 1
  }

instance ActionRunner env => HasActions env BeginnersLuck where
  getActions iid (WhenRevealToken You _) (BeginnersLuck x) =
    pure [ActivateCardAbilityAction iid (ability x)]
  getActions iid window (BeginnersLuck x) = getActions iid window x

instance
  ( HasQueue env
  , HasList Token env ()
  , HasCount PlayerCount env ()
  , HasCount SpendableClueCount env ()
  , HasSet InvestigatorId env ()
  , HasId LeadInvestigatorId env ()
  , HasSet InScenarioInvestigatorId env ()
  )
  => RunMessage env BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 4)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs]
        ]
      pure $ BeginnersLuck $ attrs & sequenceL .~ Act 1 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> a <$ unshiftMessages
      [ PlaceLocation "02074"
      , DiscardEncounterUntilFirst
        (toSource attrs)
        (EncounterCardMatchByType (EnemyType, Just Criminal))
      , NextAct aid "02067"
      ]
    RequestedEncounterCard source (Just ec) | isSource attrs source ->
      a <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) "02074")
    UseCardAbility iid source Nothing 1 payments | isSource attrs source -> do
      tokensInBag <- getList @Token ()
      a <$ unshiftMessages
        [ FocusTokens tokensInBag
        , chooseOne
          iid
          [ TargetLabel
              (TokenFaceTarget token)
              [ UseCardAbility
                  iid
                  source
                  (Just . TargetMetadata $ TokenFaceTarget token)
                  1
                  payments
              ]
          | token <- tokensInBag
          ]
        ]
    UseCardAbility iid source (Just (TargetMetadata (TokenFaceTarget token))) 1 _
      | isSource attrs source
      -> do
        replaceToken token
        a <$ unshiftMessages [FocusTokens [token], Remember $ Cheated iid]
    After (GainClues _ _) -> do
      totalClues <- unSpendableClueCount <$> getCount ()
      requiredClues <- getPlayerCountValue (PerPlayer 4)
      a <$ when
        (totalClues >= requiredClues)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    _ -> BeginnersLuck <$> runMessage msg attrs
