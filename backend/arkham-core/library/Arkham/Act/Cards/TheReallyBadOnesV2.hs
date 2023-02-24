module Arkham.Act.Cards.TheReallyBadOnesV2
  ( TheReallyBadOnesV2(..)
  , theReallyBadOnesV2
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Trait

newtype TheReallyBadOnesV2 = TheReallyBadOnesV2 ActAttrs
  deriving anyclass (IsAct, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theReallyBadOnesV2 :: ActCard TheReallyBadOnesV2
theReallyBadOnesV2 =
  act (2, A) TheReallyBadOnesV2 Cards.theReallyBadOnesV2 Nothing

instance HasModifiersFor TheReallyBadOnesV2 where
  getModifiersFor (LocationTarget lid) (TheReallyBadOnesV2 attrs) = do
    targets <- select UnrevealedLocation
    pure
      [ toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)
      | lid `member` targets
      ]
  getModifiersFor _ _ = pure []

instance RunMessage TheReallyBadOnesV2 where
  runMessage msg a@(TheReallyBadOnesV2 attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      danielsCell <- getJustLocationIdByName
        ("Patient Confinement" <:> "Daniel's Cell")
      danielChesterfield <- getSetAsideCard Enemies.danielChesterfield
      a <$ pushAll
        [ CreateEnemyAt danielChesterfield danielsCell Nothing
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    _ -> TheReallyBadOnesV2 <$> runMessage msg attrs
