module Arkham.Types.Act.Cards.TheReallyBadOnesV2
  ( TheReallyBadOnesV2(..)
  , theReallyBadOnesV2
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheReallyBadOnesV2 = TheReallyBadOnesV2 ActAttrs
  deriving anyclass (IsAct, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theReallyBadOnesV2 :: ActCard TheReallyBadOnesV2
theReallyBadOnesV2 =
  act (2, A) TheReallyBadOnesV2 Cards.theReallyBadOnesV2 Nothing

instance HasModifiersFor env TheReallyBadOnesV2 where
  getModifiersFor _ (LocationTarget _) (TheReallyBadOnesV2 attrs) = do
    pure [toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage env TheReallyBadOnesV2 where
  runMessage msg a@(TheReallyBadOnesV2 attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      danielsCell <- getJustLocationIdByName
        ("Patient Confinement" <:> "Daniel's Cell")
      danielChesterfield <- getSetAsideCard Enemies.danielChesterfield
      a <$ push (CreateEnemyAt danielChesterfield danielsCell Nothing)
    _ -> TheReallyBadOnesV2 <$> runMessage msg attrs
