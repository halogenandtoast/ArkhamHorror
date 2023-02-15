module Arkham.Treachery.Cards.Entombed
  ( entombed
  , Entombed(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { difficultyReduction :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Entombed = Entombed (TreacheryAttrs `With` Metadata)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance RunMessage Entombed where
  runMessage msg t@(Entombed (attrs `With` metadata)) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId attrs) (idToTarget iid)
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        difficulty = max 0 (4 - difficultyReduction metadata)
        testChoice sType = SkillLabel
          sType
          [beginSkillTest iid source (idToTarget iid) Nothing sType difficulty]
      push $ chooseOne iid [testChoice SkillAgility, testChoice SkillCombat]
      pure t
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        pure $ Entombed $ attrs `With` Metadata
          (difficultyReduction metadata + 1)
    EndRound -> do
      pure $ Entombed $ attrs `With` Metadata 0
    _ -> Entombed . (`with` metadata) <$> runMessage msg attrs
