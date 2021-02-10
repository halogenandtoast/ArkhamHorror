module Arkham.Types.Treachery.Cards.UmordhothsWrath where

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
import Arkham.Types.Treachery.Runner

newtype UmordhothsWrath = UmordhothsWrath TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsWrath :: TreacheryId -> a -> UmordhothsWrath
umordhothsWrath uuid _ = UmordhothsWrath $ baseAttrs uuid "01158"

instance HasModifiersFor env UmordhothsWrath where
  getModifiersFor = noModifiersFor

instance HasActions env UmordhothsWrath where
  getActions i window (UmordhothsWrath attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env UmordhothsWrath where
  runMessage msg t@(UmordhothsWrath attrs@TreacheryAttrs {..}) = case msg of
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t
      <$ unshiftMessage (HandlePointOfFailure iid (TreacheryTarget tid) n)
    HandlePointOfFailure _ (TreacheryTarget tid) 0 | tid == treacheryId ->
      pure t
    HandlePointOfFailure iid (TreacheryTarget tid) n | tid == treacheryId -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' > 0
        then t <$ unshiftMessages
          [ chooseOne
            iid
            [ Label "Discard a card from your hand" [RandomDiscard iid]
            , Label
              "Take 1 damage and 1 horror"
              [ InvestigatorAssignDamage
                  iid
                  (TreacherySource treacheryId)
                  DamageAny
                  1
                  1
              ]
            ]
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
        else t <$ unshiftMessages
          [ InvestigatorAssignDamage
            iid
            (TreacherySource treacheryId)
            DamageAny
            1
            1
          , HandlePointOfFailure iid (TreacheryTarget treacheryId) (n - 1)
          ]
    Revelation iid source | isSource attrs source -> t <$ unshiftMessage
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        5
      )
    _ -> UmordhothsWrath <$> runMessage msg attrs
