module Arkham.Location.Cards.TearThroughSpace (
  tearThroughSpace,
  TearThroughSpace (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards (tearThroughSpace)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Name
import Arkham.Timing qualified as Timing
import Control.Monad.Extra (findM)

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

tearThroughSpace :: LocationCard TearThroughSpace
tearThroughSpace =
  location TearThroughSpace Cards.tearThroughSpace 1 (Static 1)

instance HasAbilities TearThroughSpace where
  getAbilities (TearThroughSpace attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1 $ ForcedAbility $ RoundEnds Timing.When
        ]

instance RunMessage TearThroughSpace where
  runMessage msg l@(TearThroughSpace attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "Place 1 doom on Tear through Space"
              [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1]
          , Label "Discard Tear through Space" [toDiscard (toAbilitySource attrs 1) attrs]
          ]
      pure l
    Revelation _ source | isSource attrs source -> do
      let
        labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 4]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . TearThroughSpace $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    _ -> TearThroughSpace <$> runMessage msg attrs
