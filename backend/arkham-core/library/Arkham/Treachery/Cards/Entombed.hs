module Arkham.Treachery.Cards.Entombed (
  entombed,
  Entombed (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata {difficultyReduction :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype Entombed = Entombed (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

entombed :: TreacheryCard Entombed
entombed = treachery (Entombed . (`With` Metadata 0)) Cards.entombed

instance HasModifiersFor Entombed where
  getModifiersFor (InvestigatorTarget iid) (Entombed (attrs `With` _)) =
    if treacheryOnInvestigator iid attrs
      then pure $ toModifiers attrs [CannotMove, CannotDisengageEnemies]
      else pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Entombed where
  getAbilities (Entombed (a `With` _)) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ActionAbility []
        $ ActionCost 1
    ]

instance RunMessage Entombed where
  runMessage msg t@(Entombed (attrs `With` metadata)) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        difficulty = max 0 (4 - difficultyReduction metadata)
        testChoice sType =
          SkillLabel
            sType
            [beginSkillTest iid source iid sType difficulty]
      player <- getPlayer iid
      push $ chooseOne player [testChoice #agility, testChoice #combat]
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      pure
        $ Entombed
        $ attrs
        `With` Metadata
          (difficultyReduction metadata + 1)
    EndRound -> do
      pure $ Entombed $ attrs `With` Metadata 0
    _ -> Entombed . (`with` metadata) <$> runMessage msg attrs
