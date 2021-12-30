module Arkham.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Timing qualified as Timing

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor env LolaHayes where
  getModifiersFor _ target (LolaHayes attrs) | isTarget attrs target =
    pure $ toModifiers attrs [CanOnlyUseCardsInRole $ investigatorClass attrs]
  getModifiersFor _ _ _ = pure []

lolaHayes :: InvestigatorCard LolaHayes
lolaHayes = investigator
  LolaHayes
  Cards.lolaHayes
  Stats
    { health = 6
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }

instance HasTokenValue env LolaHayes where
  getTokenValue (LolaHayes attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance HasAbilities LolaHayes where
  getAbilities (LolaHayes attrs) =
    [ restrictedAbility attrs 1 Self $ ForcedAbility $ DrawingStartingHand
      Timing.After
      You
    , restrictedAbility attrs 2 Self (FastAbility Free)
      & (abilityLimitL .~ PlayerLimit PerRound 1)
    ]

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
