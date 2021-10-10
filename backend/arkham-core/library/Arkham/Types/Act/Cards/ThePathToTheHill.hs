module Arkham.Types.Act.Cards.ThePathToTheHill
  ( ThePathToTheHill(..)
  , thePathToTheHill
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Target

newtype ThePathToTheHill = ThePathToTheHill ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePathToTheHill :: ActCard ThePathToTheHill
thePathToTheHill = act (1, A) ThePathToTheHill Cards.thePathToTheHill Nothing

instance HasAbilities ThePathToTheHill where
  getAbilities (ThePathToTheHill x) =
    [ mkAbility x 1 $ Objective $ ForcedAbilityWithCost
        AnyWindow
        (GroupClueCost (PerPlayer 2) Anywhere)
    ]

instance ActRunner env => RunMessage env ThePathToTheHill where
  runMessage msg a@(ThePathToTheHill attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      locationIds <- getSetList ()
      ascendingPathId <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Ascending Path")
      a <$ pushAll
        (map (RemoveAllClues . LocationTarget) locationIds
        ++ [ RevealLocation Nothing ascendingPathId
           , AdvanceActDeck actDeckId (toSource attrs)
           ]
        )
    _ -> ThePathToTheHill <$> runMessage msg attrs
