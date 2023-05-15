module Arkham.Location.Cards.ChapelAttic_175 (
  chapelAttic_175,
  ChapelAttic_175 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ChapelAttic_175 = ChapelAttic_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAttic_175 :: LocationCard ChapelAttic_175
chapelAttic_175 = location ChapelAttic_175 Cards.chapelAttic_175 4 (Static 0)

instance HasAbilities ChapelAttic_175 where
  getAbilities (ChapelAttic_175 a) =
    withRevealedAbilities
      a
      [ restrictedAbility a 1 Here $
          ForcedAbility $
            DrawCard Timing.After You (BasicCardMatch NonWeakness) (DeckOf You)
      , mkAbility a 2 $
          ReactionAbility
            ( SkillTestResult
                Timing.After
                You
                (WhileInvestigating $ LocationWithId $ toId a)
                (SuccessResult AnyValue)
            )
            Free
      ]

toDrawn :: [Window] -> Card
toDrawn [] = error "invalid call"
toDrawn ((windowType -> Window.DrawCard _ card _) : _) = card
toDrawn (_ : xs) = toDrawn xs

instance RunMessage ChapelAttic_175 where
  runMessage msg l@(ChapelAttic_175 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelAtticSpectral_175
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 (toDrawn -> card) _ -> do
      push $ PlaceUnderneath (toTarget attrs) [card]
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      iids <- getInvestigatorIds
      let
        returnCards iid =
          let
            cards =
              filter (maybe False ((== Just iid) . pcOwner) . preview _PlayerCard) (locationCardsUnderneath attrs)
          in
            guard (notNull cards) $> AddToHand iid cards
      pushAll $ mapMaybe returnCards iids
      pure l
    _ -> ChapelAttic_175 <$> runMessage msg attrs
