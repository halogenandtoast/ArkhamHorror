module Arkham.Investigator.Cards.JimCulver where

import Arkham.Prelude

import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Message
import Arkham.Source

newtype JimCulver = JimCulver InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimCulver :: InvestigatorCard JimCulver
jimCulver = investigator
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
  getModifiersFor (TokenTarget token) (JimCulver attrs)
    | tokenFace token == Skull = do
      mSkillTestSource <- getSkillTestSource
      case mSkillTestSource of
        Just (SkillTestSource iid _ _ _) | iid == toId attrs ->
          pure $ toModifiers attrs [ChangeTokenModifier $ PositiveModifier 0]
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasTokenValue JimCulver where
  getTokenValue iid ElderSign (JimCulver attrs) | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage JimCulver where
  runMessage msg i@(JimCulver attrs@InvestigatorAttrs {..}) = case msg of
    When (RevealToken _ iid token)
      | iid == investigatorId && tokenFace token == ElderSign -> do
        i <$ push
          (chooseOne
            iid
            [ Label "Resolve as Elder Sign" []
            , Label
              "Resolve as Skull"
              [ CreateTokenEffect
                  (EffectModifiers
                  $ toModifiers attrs [TokenFaceModifier [Skull]]
                  )
                  (toSource attrs)
                  token
              ]
            ]
          )
    _ -> JimCulver <$> runMessage msg attrs
