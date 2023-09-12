module Arkham.Investigator.Cards.HarveyWalters (
  harveyWalters,
  HarveyWalters (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HarveyWalters = HarveyWalters InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harveyWalters :: InvestigatorCard HarveyWalters
harveyWalters =
  investigator
    HarveyWalters
    Cards.harveyWalters
    Stats
      { health = 7
      , sanity = 8
      , willpower = 4
      , intellect = 5
      , combat = 1
      , agility = 2
      }

instance HasAbilities HarveyWalters where
  getAbilities (HarveyWalters a) =
    [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility a 1 Self
        $ ReactionAbility
          ( DrawCard
              Timing.After
              (InvestigatorAt YourLocation)
              (BasicCardMatch AnyCard)
              AnyDeck
          )
          Free
    ]

instance HasChaosTokenValue HarveyWalters where
  getChaosTokenValue iid ElderSign (HarveyWalters attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage HarveyWalters where
  runMessage msg i@(HarveyWalters attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (map windowType -> [Window.DrawCard iid' _ _]) _ -> do
      pushM $ drawCards iid' (toAbilitySource attrs 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pushM $ drawCards iid (ChaosTokenEffectSource ElderSign) 1
      pure i
    _ -> HarveyWalters <$> runMessage msg attrs
