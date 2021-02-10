module Arkham.Types.Investigator.Cards.DaisyWalker
  ( DaisyWalker(..)
  , daisyWalker
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


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env DaisyWalker where
  getModifiersFor source target (DaisyWalker attrs) =
    getModifiersFor source target attrs

daisyWalker :: DaisyWalker
daisyWalker =
  DaisyWalker $ (baseAttrs "01002" "Daisy Walker" Seeker stats [Miskatonic])
    { investigatorTomeActions = Just 1
    }
 where
  stats = Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }

instance ActionRunner env => HasActions env DaisyWalker where
  getActions i window (DaisyWalker attrs) = getActions i window attrs

instance HasTokenValue env DaisyWalker where
  getTokenValue (DaisyWalker attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue (DaisyWalker attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env DaisyWalker where
  runMessage msg i@(DaisyWalker attrs@InvestigatorAttrs {..}) = case msg of
    ResetGame -> do
      attrs' <- runMessage msg attrs
      pure $ DaisyWalker $ attrs' { investigatorTomeActions = Just 1 }
    SpendActions iid (AssetSource aid) actionCost
      | iid == toId attrs && actionCost > 0 -> do
        isTome <- elem Tome <$> getSet aid
        if isTome && fromJustNote "Must be set" investigatorTomeActions > 0
          then DaisyWalker <$> runMessage
            (SpendActions iid (AssetSource aid) (actionCost - 1))
            (attrs
              { investigatorTomeActions =
                max 0 . subtract 1 <$> investigatorTomeActions
              }
            )
          else DaisyWalker <$> runMessage msg attrs
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | iid == investigatorId -> case drawnTokenFace token of
        ElderSign -> do
          tomeCount <- unAssetCount <$> getCount (iid, [Tome])
          i <$ when
            (tomeCount > 0)
            (unshiftMessage $ chooseOne
              iid
              [ DrawCards iid tomeCount False
              , Continue "Do not use Daisy's ability"
              ]
            )
        _ -> pure i
    BeginRound -> DaisyWalker
      <$> runMessage msg (attrs { investigatorTomeActions = Just 1 })
    _ -> DaisyWalker <$> runMessage msg attrs
