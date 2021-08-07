module Arkham.Types.Investigator.Cards.MarkHarrigan
  ( markHarrigan
  , MarkHarrigan(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MarkHarrigan = MarkHarrigan InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env MarkHarrigan where
  getModifiersFor source target (MarkHarrigan attrs) =
    getModifiersFor source target attrs

markHarrigan :: MarkHarrigan
markHarrigan =
  MarkHarrigan $ base & startsWithL .~ [Assets.sophieInLovingMemory]
 where
  base = baseAttrs
    "03001"
    "Mark Harrigan"
    Guardian
    Stats
      { willpower = 3
      , intellect = 2
      , combat = 5
      , agility = 3
      , health = 9
      , sanity = 5
      }
    [Veteran]

ability :: InvestigatorAttrs -> Ability
ability attrs =
  mkAbility attrs 1 (ReactionAbility Free) & abilityLimitL .~ PlayerLimit
    PerPhase
    1

instance InvestigatorRunner env => HasActions env MarkHarrigan where
  getActions i (WhenDealtDamage _ target) (MarkHarrigan attrs)
    | isTarget attrs target && i == toId attrs = pure [ability attrs]
  getActions i (WhenDealtDamage _ (AssetTarget aid)) (MarkHarrigan attrs)
    | aid `elem` investigatorAssets attrs && i == toId attrs = pure
      [ability attrs]
  getActions i window (MarkHarrigan attrs) = getActions i window attrs

instance HasTokenValue env MarkHarrigan where
  getTokenValue (MarkHarrigan attrs) iid ElderSign | iid == toId attrs = do
    let tokenValue' = PositiveModifier $ investigatorHealthDamage attrs
    pure $ TokenValue ElderSign tokenValue'
  getTokenValue (MarkHarrigan attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env MarkHarrigan where
  runMessage msg i@(MarkHarrigan attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      i <$ push (DrawCards iid 1 False)
    _ -> MarkHarrigan <$> runMessage msg attrs
