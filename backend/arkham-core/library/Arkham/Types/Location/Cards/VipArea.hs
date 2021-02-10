module Arkham.Types.Location.Cards.VipArea
  ( vipArea
  , VipArea(..)
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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Phase
import Arkham.Types.Trait hiding (Cultist)

newtype VipArea = VipArea LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vipArea :: VipArea
vipArea = VipArea
  $ base { locationVictory = Just 1, locationRevealedSymbol = Plus }
 where
  base = baseAttrs
    "02076"
    (Name "VIP Area" Nothing)
    EncounterSet.TheHouseAlwaysWins
    3
    (PerPlayer 1)
    T
    [Diamond]
    [CloverClub]

instance HasPhase env => HasModifiersFor env VipArea where
  getModifiersFor _ (InvestigatorTarget iid) (VipArea attrs)
    | iid `member` locationInvestigators attrs = do
      phase <- getPhase <$> ask
      if phase == UpkeepPhase
        then pure $ toModifiers attrs [CannotDrawCards, CannotGainResources]
        else pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env VipArea where
  getActions iid window (VipArea attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env VipArea where
  runMessage msg (VipArea attrs@LocationAttrs {..}) = VipArea <$> runMessage msg attrs
