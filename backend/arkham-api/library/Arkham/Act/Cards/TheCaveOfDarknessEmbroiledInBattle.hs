module Arkham.Act.Cards.TheCaveOfDarknessEmbroiledInBattle (
  TheCaveOfDarknessEmbroiledInBattle (..),
  theCaveOfDarknessEmbroiledInBattle,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Trait

newtype TheCaveOfDarknessEmbroiledInBattle = TheCaveOfDarknessEmbroiledInBattle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheCaveOfDarknessEmbroiledInBattle where
  getAbilities (TheCaveOfDarknessEmbroiledInBattle attrs) =
    [ restrictedAbility
        attrs
        999
        (exists $ "Black Cave" <> LocationWithoutClues)
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) "Black Cave"
    ]

theCaveOfDarknessEmbroiledInBattle
  :: ActCard TheCaveOfDarknessEmbroiledInBattle
theCaveOfDarknessEmbroiledInBattle =
  act (2, E) TheCaveOfDarknessEmbroiledInBattle Cards.theCaveOfDarknessEmbroiledInBattle Nothing

instance RunMessage TheCaveOfDarknessEmbroiledInBattle where
  runMessage msg a@(TheCaveOfDarknessEmbroiledInBattle attrs) = runQueueT $ case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      deckCount <- getActDecksInPlayCount
      lead <- getLead
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardUntilFirst lead (toSource attrs) Deck.EncounterDeck $ BasicCardMatch $ CardWithTrait Cultist
          ]
        <> [ DiscardUntilFirst lead (toSource attrs) Deck.EncounterDeck $ BasicCardMatch $ CardWithTrait Cultist
           | deckCount <= 2
           ]
        <> [AdvanceToAct (actDeckId attrs) Acts.theBrotherhoodIsRevealed E (toSource attrs)]
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      blackCave <- selectJust $ locationIs Locations.blackCave
      enemyId <- createEnemyAt (EncounterCard ec) blackCave
      rememberIchtacasPrey enemyId (EncounterCard ec)
      pure a
    _ -> TheCaveOfDarknessEmbroiledInBattle <$> liftRunMessage msg attrs
