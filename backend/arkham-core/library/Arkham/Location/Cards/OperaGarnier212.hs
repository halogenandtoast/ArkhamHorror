module Arkham.Location.Cards.OperaGarnier212 (
  operaGarnier212,
  OperaGarnier212 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype OperaGarnier212 = OperaGarnier212 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

operaGarnier212 :: LocationCard OperaGarnier212
operaGarnier212 =
  location OperaGarnier212 Cards.operaGarnier212 5 (PerPlayer 1)

instance HasModifiersFor OperaGarnier212 where
  getModifiersFor (CardIdTarget _) (OperaGarnier212 attrs) = do
    mtarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    case (mAction, mtarget) of
      (Just Action.Investigate, Just target)
        | isTarget attrs target ->
            pure $ toModifiers attrs [DoubleSkillIcons]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage OperaGarnier212 where
  runMessage msg (OperaGarnier212 attrs) =
    OperaGarnier212 <$> runMessage msg attrs
