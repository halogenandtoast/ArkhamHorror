module Arkham.Act.Cards.InLostCarcosa
  ( InLostCarcosa(..)
  , inLostCarcosa
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message

newtype InLostCarcosa = InLostCarcosa ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inLostCarcosa :: ActCard InLostCarcosa
inLostCarcosa = act (1, A) InLostCarcosa Cards.inLostCarcosa Nothing

instance HasAbilities InLostCarcosa where
  getAbilities (InLostCarcosa x) = withBaseAbilities x $ if onSide A x
    then
      [ mkAbility x 1 $ Objective $ ForcedAbilityWithCost
          AnyWindow
          (GroupClueCost (PerPlayer 2) Anywhere)
      ]
    else []

instance RunMessage InLostCarcosa where
  runMessage msg a@(InLostCarcosa attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId a) source AdvancedWithClues)
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      theManInThePallidMask <- getSetAsideCard Enemies.theManInThePallidMask
      palaceOfTheKing <- getJustLocationIdByName "Palace of the King"
      pushAll
        [ CreateEnemyAt theManInThePallidMask palaceOfTheKing Nothing
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> InLostCarcosa <$> runMessage msg attrs
