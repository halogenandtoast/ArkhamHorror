module Arkham.Investigator.Cards.ShatteredSelf (ShatteredSelf (..), ShatteredSelfMetadata (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (getHandCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Data.Aeson (Result (..))

newtype ShatteredSelfMetadata = ShatteredSelfMetadata {originalBody :: Value}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype ShatteredSelf = ShatteredSelf (InvestigatorAttrs `With` ShatteredSelfMetadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator ShatteredSelf where
  investigatorFromAttrs attrs = ShatteredSelf $ attrs `with` ShatteredSelfMetadata (toJSON attrs)

instance HasModifiersFor ShatteredSelf where
  getModifiersFor (ShatteredSelf (a `With` _)) = do
    n <- min 5 <$> getHandCount a.id
    modifySelf
      a
      [ HandSize (-3)
      , BaseSkillOf #willpower n
      , BaseSkillOf #intellect n
      , BaseSkillOf #combat n
      , BaseSkillOf #agility n
      ]

instance HasAbilities ShatteredSelf where
  getAbilities (ShatteredSelf a) =
    [ playerLimit PerTest
        $ selfAbility a 1 DuringYourSkillTest
        $ freeReaction
        $ CommittedCard #after You #any
    ]

instance HasChaosTokenValue ShatteredSelf where
  getChaosTokenValue iid ElderSign (ShatteredSelf (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ShatteredSelf where
  runMessage msg i@(ShatteredSelf (attrs `With` meta)) = runQueueT $ case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawCards iid ElderSign 1
      pure i
    UseCardAbility _iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure i
    _ -> case fromJSON @Investigator (originalBody meta) of
      Error _ -> error "the true self is lost"
      Success original -> do
        original' <- liftRunMessage (Blanked msg) (overAttrs (const attrs) original)
        pure $ ShatteredSelf $ toAttrs original' `with` meta
