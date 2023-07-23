module Arkham.Investigator.Cards.JimCulver where

import Arkham.Prelude

import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Message

newtype JimCulver = JimCulver InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimCulver :: InvestigatorCard JimCulver
jimCulver =
  investigator
    JimCulver
    Cards.jimCulver
    Stats
      { health = 7
      , sanity = 8
      , willpower = 4
      , intellect = 3
      , combat = 3
      , agility = 2
      }

instance HasModifiersFor JimCulver where
  getModifiersFor (ChaosTokenTarget (chaosTokenFace -> Skull)) (JimCulver attrs) = do
    miid <- getSkillTestInvestigator
    pure $ case miid of
      Just iid | iid == toId attrs -> do
        toModifiers attrs [ChangeChaosTokenModifier $ PositiveModifier 0]
      _ -> []
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue JimCulver where
  getChaosTokenValue iid ElderSign (JimCulver attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JimCulver where
  runMessage msg i@(JimCulver attrs@InvestigatorAttrs {..}) = case msg of
    When (RevealChaosToken _ iid token) | iid == investigatorId -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        push $
          chooseOne
            iid
            [ Label "Resolve as Elder Sign" []
            , Label
                "Resolve as Skull"
                [ CreateChaosTokenEffect
                    ( EffectModifiers $
                        toModifiers attrs [ChaosTokenFaceModifier [Skull]]
                    )
                    (toSource attrs)
                    token
                ]
            ]
      pure i
    _ -> JimCulver <$> runMessage msg attrs
