module Arkham.Types.Treachery.Cards.BeyondTheVeil
  ( BeyondTheVeil(..)
  , beyondTheVeil
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
import Arkham.Types.Treachery.Runner

newtype BeyondTheVeil = BeyondTheVeil TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheVeil :: TreacheryId -> a -> BeyondTheVeil
beyondTheVeil uuid _ = BeyondTheVeil $ baseAttrs uuid "02084"

instance HasModifiersFor env BeyondTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env BeyondTheVeil where
  getActions i window (BeyondTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BeyondTheVeil where
  runMessage msg t@(BeyondTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      exemptInvestigators <- getSet @InvestigatorId
        (TreacheryCardCode treacheryCardCode)
      t <$ if iid `member` exemptInvestigators
        then unshiftMessage (Discard $ toTarget attrs)
        else unshiftMessage
          (AttachTreachery treacheryId (InvestigatorTarget iid))
    DeckHasNoCards iid | treacheryOnInvestigator iid attrs ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 10 0
        , Discard $ toTarget attrs
        ]
    _ -> BeyondTheVeil <$> runMessage msg attrs
