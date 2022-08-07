module Arkham.Location.Cards.NotreDame
  ( notreDame
  , NotreDame(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype NotreDame = NotreDame LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notreDame :: LocationCard NotreDame
notreDame = location NotreDame Cards.notreDame 3 (PerPlayer 1)

instance HasModifiersFor NotreDame where
  getModifiersFor (EnemyTarget eid) (NotreDame attrs)
    | eid `member` locationEnemies attrs && locationRevealed attrs
    = pure $ toModifiers attrs [EnemyFight (-1), EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities NotreDame where
  getAbilities (NotreDame attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerGame 1)
      $ restrictedAbility attrs 1 Here
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance RunMessage NotreDame where
  runMessage msg l@(NotreDame attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 6)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        agenda <- selectJust AnyAgenda
        hasDoom <- agendaMatches agenda AgendaWithAnyDoom
        l <$ push
          (chooseOrRunOne iid
          $ Label
              "Place 1 doom on current agenda"
              [PlaceDoom (AgendaTarget agenda) 1]
          : [ Label
                "Remove 1 doom on current agenda"
                [RemoveDoom (AgendaTarget agenda) 1]
            | hasDoom
            ]
          )
    _ -> NotreDame <$> runMessage msg attrs
