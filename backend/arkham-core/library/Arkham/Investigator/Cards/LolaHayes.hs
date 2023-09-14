module Arkham.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LolaHayes where
  getModifiersFor target (LolaHayes attrs) | isTarget attrs target = do
    pure $ toModifiers attrs [CanOnlyUseCardsInRole $ investigatorClass attrs]
  getModifiersFor _ _ = pure []

lolaHayes :: InvestigatorCard LolaHayes
lolaHayes =
  investigator LolaHayes Cards.lolaHayes
    $ Stats {health = 6, sanity = 6, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasChaosTokenValue LolaHayes where
  getChaosTokenValue iid ElderSign (LolaHayes attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities LolaHayes where
  getAbilities (LolaHayes attrs) =
    [ restrictedAbility attrs 1 Self $ ForcedAbility $ DrawingStartingHand Timing.After You
    , playerLimit PerRound $ restrictedAbility attrs 2 Self (FastAbility Free)
    ]

switchRole :: HasQueue Message m => InvestigatorAttrs -> m ()
switchRole attrs =
  let roles = filter (/= Mythos) [minBound .. maxBound]
   in push $ chooseOne attrs.id [Label (tshow role) [SetRole attrs.id role] | role <- roles]

instance RunMessage LolaHayes where
  runMessage msg i@(LolaHayes attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) n _ _ | n `elem` [1, 2] -> do
      switchRole attrs
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      switchRole attrs
      pure i
    _ -> LolaHayes <$> runMessage msg attrs
