module Arkham.Act.Cards.SearchingForTheTome (
  SearchingForTheTome (..),
  searchingForTheTome,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher

newtype SearchingForTheTome = SearchingForTheTome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

searchingForTheTome :: ActCard SearchingForTheTome
searchingForTheTome =
  act (3, A) SearchingForTheTome Cards.searchingForTheTome Nothing

instance HasAbilities SearchingForTheTome where
  getAbilities (SearchingForTheTome x)
    | onSide A x =
        [ restrictedAbility
            x
            1
            ( LocationExists
                $ locationIs Cards.exhibitHallRestrictedHall
                <> LocationWithoutClues
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage SearchingForTheTome where
  runMessage msg a@(SearchingForTheTome attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      push
        $ chooseOne
          lead
          [ Label "It's too dangerous to keep around. We have to destroy it. (-> R1)" [R1]
          , Label "It's too valuable to destroy. We have to keep it safe. (-> R2)" [R2]
          ]
      pure a
    _ -> SearchingForTheTome <$> runMessage msg attrs
