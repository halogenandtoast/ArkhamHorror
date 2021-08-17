module Arkham.Types.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Stats
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env LolaHayes where
  getModifiersFor _ target (LolaHayes attrs) | isTarget attrs target =
    pure $ toModifiers attrs [CanOnlyUseCardsInRole $ investigatorClass attrs]
  getModifiersFor source target (LolaHayes attrs) =
    getModifiersFor source target attrs

lolaHayes :: LolaHayes
lolaHayes = LolaHayes $ baseAttrs
  "03006"
  "Lola Hayes"
  Neutral
  Stats
    { health = 6
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Performer]

instance HasTokenValue env LolaHayes where
  getTokenValue (LolaHayes attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (LolaHayes attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => HasAbilities env LolaHayes where
  getAbilities i (Window Timing.After (DrawingStartingHand iid)) (LolaHayes attrs)
    | iid == toId attrs && i == iid
    = pure [mkAbility attrs 1 LegacyForcedAbility]
  getAbilities i (Window Timing.When FastPlayerWindow) (LolaHayes attrs)
    | i == toId attrs = pure
      [ mkAbility attrs 2 (FastAbility Free)
          & (abilityLimitL .~ PlayerLimit PerRound 1)
      ]
  getAbilities i window (LolaHayes attrs) = getAbilities i window attrs

switchRole
  :: (MonadIO m, MonadReader env m, HasQueue env) => InvestigatorAttrs -> m ()
switchRole attrs = push
  (chooseOne
    (toId attrs)
    [ Label (tshow role) [SetRole (toId attrs) role]
    | role <- [minBound .. maxBound]
    ]
  )

instance (InvestigatorRunner env) => RunMessage env LolaHayes where
  runMessage msg i@(LolaHayes attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      i <$ switchRole attrs
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      i <$ switchRole attrs
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      i <$ switchRole attrs
    _ -> LolaHayes <$> runMessage msg attrs
