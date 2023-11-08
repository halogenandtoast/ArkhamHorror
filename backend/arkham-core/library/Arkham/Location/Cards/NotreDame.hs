module Arkham.Location.Cards.NotreDame (
  notreDame,
  NotreDame (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype NotreDame = NotreDame LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notreDame :: LocationCard NotreDame
notreDame = location NotreDame Cards.notreDame 3 (PerPlayer 1)

instance HasModifiersFor NotreDame where
  getModifiersFor (EnemyTarget eid) (NotreDame attrs) | locationRevealed attrs = do
    atLocation <- enemyAtLocation eid attrs
    pure $ toModifiers attrs $ guard atLocation *> [EnemyFight (-1), EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities NotreDame where
  getAbilities (NotreDame attrs) =
    withRevealedAbilities
      attrs
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage NotreDame where
  runMessage msg l@(NotreDame attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source attrs SkillWillpower 6
      pure l
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      agenda <- selectJust AnyAgenda
      hasDoom <- agendaMatches agenda AgendaWithAnyDoom
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Place 1 doom on current agenda"
          [PlaceDoom (toAbilitySource attrs 1) (toTarget agenda) 1]
        : [ Label
            "Remove 1 doom on current agenda"
            [RemoveDoom (toAbilitySource attrs 1) (toTarget agenda) 1]
          | hasDoom
          ]
      pure l
    _ -> NotreDame <$> runMessage msg attrs
