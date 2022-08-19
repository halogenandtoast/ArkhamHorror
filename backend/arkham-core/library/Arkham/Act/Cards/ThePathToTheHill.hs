module Arkham.Act.Cards.ThePathToTheHill
  ( ThePathToTheHill(..)
  , thePathToTheHill
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Target

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePathToTheHill :: ActCard ThePathToTheHill
thePathToTheHill = act (1, A) ThePathToTheHill Cards.thePathToTheHill Nothing

instance HasAbilities ThePathToTheHill where
  getAbilities (ThePathToTheHill x) | onSide A x =
    [ mkAbility x 1 $ Objective $ ForcedAbilityWithCost
        AnyWindow
        (GroupClueCost (PerPlayer 2) Anywhere)
    ]
  getAbilities _ = []

instance RunMessage ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs) AdvancedWithClues)
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      locationIds <- selectList Anywhere
      ascendingPathId <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Ascending Path")
      a <$ pushAll
        (map (RemoveAllClues . LocationTarget) locationIds
        ++ [ RevealLocation Nothing ascendingPathId
           , AdvanceActDeck actDeckId (toSource attrs)
           ]
        )
    _ -> ThePathToTheHill <$> runMessage msg attrs
