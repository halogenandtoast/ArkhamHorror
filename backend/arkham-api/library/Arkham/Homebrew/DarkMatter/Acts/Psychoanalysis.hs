module Arkham.Homebrew.DarkMatter.Acts.Psychoanalysis (psychoanalysis) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (getGrid)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern School)
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Psychoanalysis = Psychoanalysis ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychoanalysis :: ActCard Psychoanalysis
psychoanalysis = act (2, A) Psychoanalysis Cards.psychoanalysis Nothing

{- | The printed schematic, read top-left to bottom-right:

@
  circle  square    triangle
  cross   slash     moon
@

Those are the connection symbols of the six [[School]] locations
(Classroom K2, Cafeteria, Gymnasium / Library, Entrance Hall, Biology Lab);
"cross" is 'Plus' and "slash" is 'Squiggle'.
-}
schematic :: [[LocationSymbol]]
schematic =
  [ [Circle, Square, Triangle]
  , [Plus, Squiggle, Moon]
  ]

{- | True when the School locations currently sit in the printed arrangement.
Positions are normalised against the top-left-most School location so the
schematic can match anywhere on the grid.
-}
matchesSchematic :: HasGame m => m Bool
matchesSchematic = do
  locations <- select $ LocationWithTrait School
  grid <- getGrid
  placed <- for locations \lid -> do
    symbol <- field LocationPrintedSymbol lid
    pure ((,symbol) <$> findInGrid lid grid)
  case sequence placed of
    Nothing -> pure False
    Just entries
      | length entries /= 6 -> pure False
      | otherwise -> do
          let rows = map (positionRow . fst) entries
              cols = map (positionColumn . fst) entries
              top = minimumEx rows
              left = minimumEx cols
              normalised =
                [ ((positionRow p - top, positionColumn p - left), s)
                | (p, s) <- entries
                ]
              expected =
                [ ((r, c), s)
                | (r, row) <- zip [0 ..] schematic
                , (c, s) <- zip [0 ..] row
                ]
          pure $ sort normalised == sort expected

{- | "[free] Spend 1[per_investigator] clues, as a group: Switch two adjacent
locations with each other.
Objective - At the end of the round, if the configuration of [[School]]
locations correspond to the schematic, advance."
-}
instance HasAbilities Psychoanalysis where
  getAbilities (Psychoanalysis a) =
    [ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2 $ Objective $ forced $ RoundEnds #when
    ]

instance RunMessage Psychoanalysis where
  runMessage msg a@(Psychoanalysis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithTrait School
      chooseOneM iid $ targets locations \first' -> do
        adjacent <- select $ connectedFrom (LocationWithId first') <> LocationWithTrait School
        chooseOneM iid $ targets adjacent \second' ->
          push $ ScenarioSpecific "switchLocations" (toJSON (first', second'))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      whenM matchesSchematic $ advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Psychoanalysis <$> liftRunMessage msg attrs
