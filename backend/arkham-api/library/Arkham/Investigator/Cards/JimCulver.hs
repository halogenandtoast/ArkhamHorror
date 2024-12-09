module Arkham.Investigator.Cards.JimCulver where

import Arkham.ChaosToken
import Arkham.Game.Helpers (getModifiedChaosTokenFace)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype JimCulver = JimCulver InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

jimCulver :: InvestigatorCard JimCulver
jimCulver =
  investigator JimCulver Cards.jimCulver
    $ Stats {health = 7, sanity = 8, willpower = 4, intellect = 3, combat = 3, agility = 2}

instance HasModifiersFor JimCulver where
  getModifiersFor (JimCulver attrs) =
    getSkillTestInvestigator >>= \case
      Just iid | iid == attrs.id -> do
        skulls <- filter ((== #skull) . (.face)) <$> getSkillTestRevealedChaosTokens
        modifyEach attrs skulls [ChangeChaosTokenModifier (PositiveModifier 0)]
      _ -> pure mempty

instance HasChaosTokenValue JimCulver where
  getChaosTokenValue iid ElderSign (JimCulver attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JimCulver where
  runMessage msg i@(JimCulver attrs) = runQueueT $ case msg of
    When (RevealChaosToken _ iid token) | iid == attrs.id -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) do
        chooseOneM iid do
          labeled "Resolve as {elderSign}" nothing
          labeled "Resolve as {skull}" $ chaosTokenEffect attrs token $ ChaosTokenFaceModifier [Skull]
      pure i
    _ -> JimCulver <$> liftRunMessage msg attrs
