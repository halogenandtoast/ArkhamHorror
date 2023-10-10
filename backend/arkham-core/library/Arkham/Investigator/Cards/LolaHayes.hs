module Arkham.Investigator.Cards.LolaHayes where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher

newtype LolaHayes = LolaHayes InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LolaHayes where
  getModifiersFor target (LolaHayes attrs) | attrs `is` target = do
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
    [ restrictedAbility attrs 1 Self $ ForcedAbility $ DrawingStartingHand #after You
    , playerLimit PerRound $ restrictedAbility attrs 2 Self (FastAbility Free)
    ]

switchRole :: (HasGame m, HasQueue Message m) => InvestigatorAttrs -> m ()
switchRole attrs = do
  let roles = filter (/= Mythos) [minBound .. maxBound]
  player <- getPlayer (toId attrs)
  push $ chooseOne player [Label (tshow role) [SetRole attrs.id role] | role <- roles]

instance RunMessage LolaHayes where
  runMessage msg i@(LolaHayes attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [1, 2] -> do
      switchRole attrs
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      switchRole attrs
      pure i
    _ -> LolaHayes <$> runMessage msg attrs
