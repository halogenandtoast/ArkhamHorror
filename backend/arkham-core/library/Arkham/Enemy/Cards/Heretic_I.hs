module Arkham.Enemy.Cards.Heretic_I (
  heretic_I,
  Heretic_I (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait (Spectral))

newtype Metadata = Metadata {hasBeenRevealed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Heretic_I = Heretic_I (EnemyAttrs `With` Metadata)
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_I :: EnemyCard Heretic_I
heretic_I = enemy (Heretic_I . (`with` Metadata False)) Cards.heretic_I (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_I where
  getModifiersFor = hereticModifiers

instance HasAbilities Heretic_I where
  getAbilities heretic@(Heretic_I (attrs `With` meta))
    | hasBeenRevealed meta =
        let
          updateParley ability = case abilityAction ability of
            Just Action.Parley ->
              restrictedAbility attrs 1 (OnSameLocation <> OnLocation (LocationWithTrait Spectral)) $
                FastAbility $
                  ClueCost (StaticWithPerPlayer 1 1)
            _ -> ability
        in
          map updateParley (hereticAbilities heretic)
  getAbilities heretic = hereticAbilities heretic

instance RunMessage Heretic_I where
  runMessage msg e@(Heretic_I (attrs `With` meta)) = case msg of
    LookAtRevealed _ _ (isTarget attrs -> True) | hasBeenRevealed meta -> do
      push $ AddToVictory (toTarget attrs)
      pure e
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let unfinishedBusiness = lookupCard Story.unfinishedBusiness_J (toCardId attrs)
      pushAll
        [ FocusCards [unfinishedBusiness]
        , chooseOne iid [Label "Continue" [UnfocusCards]]
        ]
      pure . Heretic_I $ attrs `with` Metadata True
    _ -> hereticRunner Story.unfinishedBusiness_J msg e
