module Arkham.Investigator.Cards.FatherMateo (
  fatherMateo,
  FatherMateo (..),
  fatherMateoElderSignEffect,
) where

import Arkham.Prelude

import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window qualified as Window

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatherMateo :: InvestigatorCard FatherMateo
fatherMateo =
  investigator FatherMateo Cards.fatherMateo
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 3, combat = 2, agility = 3}

instance HasAbilities FatherMateo where
  getAbilities (FatherMateo a) =
    [ playerLimit PerGame
        $ restrictedAbility a 1 Self
        $ freeReaction (Matcher.RevealChaosToken #after (affectsOthers Anyone) #autofail)
    ]

instance HasChaosTokenValue FatherMateo where
  getChaosTokenValue iid ElderSign (FatherMateo attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateo where
  runMessage msg i@(FatherMateo attrs) = case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      pushAll [createCardEffect Cards.fatherMateo Nothing ElderSign iid, PassSkillTest]
      pure i
    UseCardAbility _ (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      push $ chaosTokenEffect (attrs.ability 1) token (ChaosTokenFaceModifier [ElderSign])
      pure i
    _ -> FatherMateo <$> runMessage msg attrs

newtype FatherMateoElderSignEffect = FatherMateoElderSignEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatherMateoElderSignEffect :: EffectArgs -> FatherMateoElderSignEffect
fatherMateoElderSignEffect = cardEffect FatherMateoElderSignEffect Cards.fatherMateo

instance RunMessage FatherMateoElderSignEffect where
  runMessage msg e@(FatherMateoElderSignEffect attrs) = case msg of
    SkillTestEnds _ _ -> do
      push $ DisableEffect $ toId attrs
      for_ (attrs.target ^? #investigator) $ \iid -> do
        isTurn <- iid <=~> TurnInvestigator
        player <- getPlayer iid
        push
          $ chooseOrRunOne player
          $ Label
            "Draw 1 card and gain 1 resource"
            [ drawCards iid ElderSign 1
            , TakeResources iid 1 (toSource ElderSign) False
            ]
          : [Label "Take an additional action this turn" [GainActions iid (toSource attrs) 1] | isTurn]
      pure e
    _ -> FatherMateoElderSignEffect <$> runMessage msg attrs
