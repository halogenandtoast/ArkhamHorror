module Arkham.Act.Cards.InLostCarcosa (
  InLostCarcosa (..),
  inLostCarcosa,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
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
  getAbilities (InLostCarcosa x) =
    withBaseAbilities
      x
      [ mkAbility x 1 $
        Objective $
          ForcedAbilityWithCost
            AnyWindow
            (GroupClueCost (PerPlayer 2) Anywhere)
      | onSide A x
      ]

instance RunMessage InLostCarcosa where
  runMessage msg a@(InLostCarcosa attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) source AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      theManInThePallidMask <- getSetAsideCard Enemies.theManInThePallidMask
      palaceOfTheKing <- getJustLocationByName "Palace of the King"

      createTheManInThePallidMask <-
        createEnemyAt_
          theManInThePallidMask
          palaceOfTheKing
          Nothing

      pushAll
        [ createTheManInThePallidMask
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> InLostCarcosa <$> runMessage msg attrs
