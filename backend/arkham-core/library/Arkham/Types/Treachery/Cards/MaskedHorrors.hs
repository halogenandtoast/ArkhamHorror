module Arkham.Types.Treachery.Cards.MaskedHorrors where

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


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryId -> a -> MaskedHorrors
maskedHorrors uuid _ = MaskedHorrors $ baseAttrs uuid "50031"

instance HasModifiersFor env MaskedHorrors where
  getModifiersFor = noModifiersFor

instance HasActions env MaskedHorrors where
  getActions i window (MaskedHorrors attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MaskedHorrors where
  runMessage msg t@(MaskedHorrors attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      iids <- getInvestigatorIds
      targetInvestigators <- map fst . filter ((>= 2) . snd) <$> for
        iids
        (\iid -> do
          clueCount <- unClueCount <$> getCount iid
          pure (iid, clueCount)
        )
      t <$ if null targetInvestigators
        then unshiftMessages
          ([ InvestigatorAssignDamage iid source DamageAny 0 2
           | iid <- targetInvestigators
           ]
          <> [Discard (TreacheryTarget treacheryId)]
          )
        else unshiftMessages
          [ PlaceDoomOnAgenda
          , AdvanceAgendaIfThresholdSatisfied
          , Discard (TreacheryTarget treacheryId)
          ]
    _ -> MaskedHorrors <$> runMessage msg attrs
