module Arkham.Types.Location.Cards.TrappersCabin
  ( TrappersCabin(..)
  , trappersCabin
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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: TrappersCabin
trappersCabin = TrappersCabin $ baseAttrs
  "81014"
  (Name "Trapper's Cabin" Nothing)
  EncounterSet.CurseOfTheRougarou
  3
  (Static 0)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance HasModifiersFor env TrappersCabin where
  getModifiersFor _ (InvestigatorTarget iid) (TrappersCabin attrs) =
    pure $ toModifiers
      attrs
      [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TrappersCabin where
  getActions iid NonFast (TrappersCabin attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      assetNotTaken <- isNothing
        <$> getId @(Maybe StoryAssetId) (CardCode "81020")
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 5])
            )
        | iid `member` locationInvestigators && assetNotTaken
        ]
  getActions i window (TrappersCabin attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        bearTrap <- PlayerCard <$> genPlayerCard "81020"
        l <$ unshiftMessage (TakeControlOfSetAsideAsset iid bearTrap)
    _ -> TrappersCabin <$> runMessage msg attrs
