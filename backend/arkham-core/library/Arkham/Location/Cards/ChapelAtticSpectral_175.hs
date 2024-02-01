module Arkham.Location.Cards.ChapelAtticSpectral_175 (
  chapelAtticSpectral_175,
  ChapelAtticSpectral_175 (..),
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

newtype ChapelAtticSpectral_175 = ChapelAtticSpectral_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

chapelAtticSpectral_175 :: LocationCard ChapelAtticSpectral_175
chapelAtticSpectral_175 = location ChapelAtticSpectral_175 Cards.chapelAtticSpectral_175 4 (Static 0)

instance HasAbilities ChapelAtticSpectral_175 where
  getAbilities (ChapelAtticSpectral_175 a) =
    withRevealedAbilities
      a
      [ restrictedAbility a 1 Here
          $ ForcedAbility
          $ DrawCard Timing.After You (BasicCardMatch NonWeakness) (DeckOf You)
      , mkAbility a 2
          $ ReactionAbility
            ( SkillTestResult
                Timing.After
                You
                (WhileInvestigating $ LocationWithId $ toId a)
                (SuccessResult AnyValue)
            )
            Free
      , withTooltip "Discard a random card from beneath Chapel Attic."
          $ restrictedAbility a 3 hauntedCriteria Haunted
      ]
   where
    hauntedCriteria = if null (locationCardsUnderneath a) then Never else NoRestriction

toDrawn :: [Window] -> Card
toDrawn [] = error "invalid call"
toDrawn ((windowType -> Window.DrawCard _ card _) : _) = card
toDrawn (_ : xs) = toDrawn xs

instance RunMessage ChapelAtticSpectral_175 where
  runMessage msg l@(ChapelAtticSpectral_175 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.chapelAttic_175
      push $ ReplaceLocation (toId attrs) regular Swap
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
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      let
        toDiscardMsg = \case
          PlayerCard pc -> case pcOwner pc of
            Just iid' -> AddToDiscard iid' pc
            Nothing -> RemovePlayerCardFromGame False (PlayerCard pc)
          EncounterCard ec -> AddToEncounterDiscard ec
          VengeanceCard _ -> error "unexpected vengeance card"

      for_ (nonEmpty $ locationCardsUnderneath attrs) $ \cards -> do
        card <- sample cards
        push $ toDiscardMsg card
      pure l
    _ -> ChapelAtticSpectral_175 <$> runMessage msg attrs
