module Arkham.Investigator.Cards.BodyOfAYithian (BodyOfAYithian (..), YithianMetadata (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Yithian))
import Data.Aeson (Result (..))

newtype YithianMetadata = YithianMetadata {originalBody :: Value}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype BodyOfAYithian = BodyOfAYithian (InvestigatorAttrs `With` YithianMetadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor BodyOfAYithian where
  getModifiersFor (BodyOfAYithian (a `With` _)) = do
    modifySelect a (AssetControlledBy (InvestigatorWithId a.id) <> #ally) [AddTrait Yithian]

instance HasAbilities BodyOfAYithian where
  getAbilities (BodyOfAYithian a) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility a 1 (Self <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ freeReaction
        $ CommittedCard #after You AnyCard
    ]

instance HasChaosTokenValue BodyOfAYithian where
  getChaosTokenValue iid ElderSign (BodyOfAYithian (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BodyOfAYithian where
  runMessage msg i@(BodyOfAYithian (attrs `With` meta)) = runQueueT $ case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      drawCardsIfCan iid ElderSign 1
      pure i
    UseCardAbility _iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure i
    _ -> do
      case fromJSON @Investigator (originalBody meta) of
        Error _ -> error "the original mind of the Yithian is lost"
        Success original -> do
          original' <- liftRunMessage (Blanked msg) (overAttrs (const attrs) original)
          pure $ BodyOfAYithian $ toAttrs original' `with` meta
