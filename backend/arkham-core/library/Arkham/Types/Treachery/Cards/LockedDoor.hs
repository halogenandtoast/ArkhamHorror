module Arkham.Types.Treachery.Cards.LockedDoor
  ( LockedDoor(..)
  , lockedDoor
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryId -> a -> LockedDoor
lockedDoor uuid _ = LockedDoor $ baseAttrs uuid "01174"

instance HasModifiersFor env LockedDoor where
  getModifiersFor _ (LocationTarget lid) (LockedDoor attrs) =
    pure $ toModifiers
      attrs
      [ CannotInvestigate | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env LockedDoor where
  getActions iid NonFast (LockedDoor a@TreacheryAttrs {..}) = do
    investigatorLocationId <- getId @LocationId iid
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
      | treacheryOnLocation investigatorLocationId a
      ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env LockedDoor where
  runMessage msg t@(LockedDoor attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptLocations <- getSet @LocationId
        (TreacheryCardCode treacheryCardCode)
      targetLocations <-
        setToList . (`difference` exemptLocations) <$> getSet @LocationId ()
      locations <- for
        targetLocations
        (traverseToSnd $ (unClueCount <$>) . getCount)
      case maxes locations of
        [] -> pure ()
        [x] -> unshiftMessages [AttachTreachery treacheryId (LocationTarget x)]
        xs -> unshiftMessage
          (chooseOne
            iid
            [ AttachTreachery treacheryId (LocationTarget x) | x <- xs ]
          )
      LockedDoor <$> runMessage msg attrs
    UseCardAbility iid (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage
        (chooseOne
          iid
          [ BeginSkillTest
            iid
            (toSource attrs)
            (toTarget attrs)
            Nothing
            SkillCombat
            4
          , BeginSkillTest
            iid
            (toSource attrs)
            (toTarget attrs)
            Nothing
            SkillAgility
            4
          ]
        )
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> LockedDoor <$> runMessage msg attrs
