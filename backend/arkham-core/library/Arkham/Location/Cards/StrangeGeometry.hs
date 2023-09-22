module Arkham.Location.Cards.StrangeGeometry (
  strangeGeometry,
  StrangeGeometry (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Name
import Control.Monad.Extra (findM)

newtype StrangeGeometry = StrangeGeometry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeGeometry :: LocationCard StrangeGeometry
strangeGeometry = location StrangeGeometry Cards.strangeGeometry 4 (Static 1)

instance HasAbilities StrangeGeometry where
  getAbilities (StrangeGeometry a) =
    withRevealedAbilities a
      $ [ mkAbility a 1 $ ForcedAbility $ PhaseEnds #after #investigation
        , restrictedAbility
            a
            2
            ( Here
                <> CluesOnThis (EqualTo $ Static 0)
                <> LocationExists (RevealedLocation <> NotLocation (LocationWithId $ toId a))
            )
            $ FastAbility Free
        ]

instance RunMessage StrangeGeometry where
  runMessage msg l@(StrangeGeometry attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ Move $ move attrs iid (toId attrs)
      let labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . StrangeGeometry $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      locationsWithMostClues <-
        selectList $ LocationWithMostClues $ NotLocation $ LocationWithId $ toId attrs
      investigators <- selectList $ investigatorAt $ toId attrs
      enemies <- selectList $ UnengagedEnemy <> enemyAt (toId attrs)
      lead <- getLead
      let source = toAbilitySource attrs 1
      pushAll
        $ [ chooseOrRunOne lead
            $ targetLabels locationsWithMostClues
            $ \lid ->
              concatMap (\iid -> [Move $ move attrs iid lid, assignDamageAndHorror iid source 1 1]) investigators
                <> [Move $ move attrs enemy lid | enemy <- enemies]
          | notNull investigators || notNull enemies
          ]
          <> [Discard (toSource attrs) (toTarget attrs)]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ Move $ moveToMatch attrs iid $ RevealedLocation <> NotLocation (LocationWithId $ toId attrs)
      pure l
    _ -> StrangeGeometry <$> runMessage msg attrs
