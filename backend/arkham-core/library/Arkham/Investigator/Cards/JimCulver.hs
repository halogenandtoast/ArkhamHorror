module Arkham.Investigator.Cards.JimCulver where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner

newtype JimCulver = JimCulver InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

jimCulver :: InvestigatorCard JimCulver
jimCulver =
  investigator JimCulver Cards.jimCulver
    $ Stats {health = 7, sanity = 8, willpower = 4, intellect = 3, combat = 3, agility = 2}

instance HasModifiersFor JimCulver where
  getModifiersFor (ChaosTokenTarget (chaosTokenFace -> Skull)) (JimCulver attrs) = do
    miid <- getSkillTestInvestigator
    pure $ toModifiers attrs $ do
      iid <- toList miid
      guard $ attrs `is` iid
      pure $ ChangeChaosTokenModifier (PositiveModifier 0)
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue JimCulver where
  getChaosTokenValue iid ElderSign (JimCulver attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JimCulver where
  runMessage msg i@(JimCulver attrs@InvestigatorAttrs {..}) = case msg of
    When (RevealChaosToken _ iid token) | iid == investigatorId -> do
      faces <- getModifiedChaosTokenFace token
      player <- getPlayer iid
      pushWhen (ElderSign `elem` faces)
        $ chooseOne player
        $ [ Label "Resolve as {elderSign}" []
          , Label "Resolve as {skull}" [chaosTokenEffect attrs token $ ChaosTokenFaceModifier [Skull]]
          ]
      pure i
    _ -> JimCulver <$> runMessage msg attrs
