module Arkham.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LolaHayes where
  getModifiersFor target (LolaHayes attrs) | isTarget attrs target =
    pure $ toModifiers attrs [CanOnlyUseCardsInRole $ investigatorClass attrs]
  getModifiersFor _ _ = pure []

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

instance HasChaosTokenValue LolaHayes where
  getChaosTokenValue iid ElderSign (LolaHayes attrs) | iid == investigatorId attrs =
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities LolaHayes where
  getAbilities (LolaHayes attrs) =
    [ restrictedAbility attrs 1 Self $ ForcedAbility $ DrawingStartingHand
      Timing.After
      You
    , limitedAbility (PlayerLimit PerRound 1)
      $ restrictedAbility attrs 2 Self (FastAbility Free)
    ]

switchRole :: HasQueue Message m => InvestigatorAttrs -> m ()
switchRole attrs = push $ chooseOne
  (toId attrs)
  [ Label (tshow role) [SetRole (toId attrs) role]
  | role <- filter (/= Mythos) [minBound .. maxBound]
  ]

instance RunMessage LolaHayes where
  runMessage msg i@(LolaHayes attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      i <$ switchRole attrs
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      i <$ switchRole attrs
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      i <$ switchRole attrs
    _ -> LolaHayes <$> runMessage msg attrs
