module Arkham.Types.Location.Cards.CloverClubBar
  ( cloverClubBar
  , CloverClubBar(..)
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
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: CloverClubBar
cloverClubBar = CloverClubBar $ baseAttrs
  "02072"
  (Name "Clover Club Bar" Nothing)
  EncounterSet.TheHouseAlwaysWins
  3
  (Static 0)
  Square
  [Triangle, Circle]
  [CloverClub]

instance HasModifiersFor env CloverClubBar where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = (mkAbility
                  (toSource attrs)
                  1
                  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2]
                  )
                )
  { abilityLimit = PlayerLimit PerGame 1
  }

instance ActionRunner env => HasActions env CloverClubBar where
  getActions iid NonFast (CloverClubBar attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ do
      step <- unActStep . getStep <$> ask
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && step == 1
        ]
  getActions iid window (CloverClubBar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubBar where
  runMessage msg l@(CloverClubBar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source && locationRevealed ->
      l <$ unshiftMessages
        [GainClues iid 2, DrawCards iid 2 False, Remember $ HadADrink iid]
    _ -> CloverClubBar <$> runMessage msg attrs
