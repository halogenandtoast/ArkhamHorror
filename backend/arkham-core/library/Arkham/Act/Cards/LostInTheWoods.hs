module Arkham.Act.Cards.LostInTheWoods
  ( LostInTheWoods(..)
  , lostInTheWoods
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype LostInTheWoods = LostInTheWoods ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LostInTheWoods where
  getModifiersFor (LocationTarget lid) (LostInTheWoods a) = do
    mInFrontOf <- field LocationInFrontOf lid
    pure $ toModifiers
      a
      [ ConnectedToWhen (LocationWithId lid)
        $ NotLocation (LocationWithId lid)
        <> LocationIsInFrontOf (InvestigatorWithId iid)
      | iid <- maybeToList mInFrontOf
      ]
  getModifiersFor (InvestigatorTarget iid) (LostInTheWoods a) = do
    lids <- selectList
      $ LocationIsInFrontOf (NotInvestigator $ InvestigatorWithId iid)
    pure $ toModifiers a $ map CannotEnter lids
  getModifiersFor _ _ = pure []

instance HasAbilities LostInTheWoods where
  getAbilities (LostInTheWoods a) =
    [ mkAbility a 1
        $ Objective
        $ ReactionAbility (RoundEnds Timing.When)
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

lostInTheWoods :: ActCard LostInTheWoods
lostInTheWoods = act (1, A) LostInTheWoods Cards.lostInTheWoods Nothing

instance RunMessage LostInTheWoods where
  runMessage msg a@(LostInTheWoods attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      arkhamWoods <- shuffleM
        =<< selectList (SetAsideCardMatch $ CardWithTitle "Arkham Woods")
      placements <- traverse placeLocation arkhamWoods

      let
        msgs =
          flip concatMap (zip iids placements) $ \(iid, (lid, placement)) ->
            [ placement
            , PutLocationInFrontOf iid lid
            , MoveTo $ uncancellableMove $ move attrs iid lid
            ]

      pushAll msgs
      pure a
    _ -> LostInTheWoods <$> runMessage msg attrs
