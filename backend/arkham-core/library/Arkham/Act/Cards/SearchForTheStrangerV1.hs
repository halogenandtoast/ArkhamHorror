module Arkham.Act.Cards.SearchForTheStrangerV1 (
  SearchForTheStrangerV1 (..),
  searchForTheStrangerV1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype SearchForTheStrangerV1 = SearchForTheStrangerV1 ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV1 :: ActCard SearchForTheStrangerV1
searchForTheStrangerV1 =
  act (2, A) SearchForTheStrangerV1 Cards.searchForTheStrangerV1 Nothing

instance HasModifiersFor SearchForTheStrangerV1 where
  getModifiersFor (EnemyTarget eid) (SearchForTheStrangerV1 a) = do
    isManInThePallidMask <-
      eid
        `isMatch` EnemyWithTitle "The Main in the Pallid Mask"
    pure $ toModifiers a [CannotBeDefeated | isManInThePallidMask]
  getModifiersFor _ _ = pure []

instance HasAbilities SearchForTheStrangerV1 where
  getAbilities (SearchForTheStrangerV1 a) =
    [ restrictedAbility
        a
        1
        ( OnLocation
            $ LocationWithEnemy
            $ enemyIs
              Enemies.theManInThePallidMask
        )
        $ Objective
        $ ActionAbility []
        $ ActionCost 3
    ]

instance RunMessage SearchForTheStrangerV1 where
  runMessage msg a@(SearchForTheStrangerV1 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push (AdvanceAct (toId attrs) source AdvancedWithOther)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      thePallidMask <- genPlayerCard Assets.thePallidMask
      pushAll
        [ addToHand leadInvestigatorId (PlayerCard thePallidMask)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> SearchForTheStrangerV1 <$> runMessage msg attrs
