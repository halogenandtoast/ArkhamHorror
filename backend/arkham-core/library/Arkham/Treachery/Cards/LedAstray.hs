module Arkham.Treachery.Cards.LedAstray (
  ledAstray,
  LedAstray (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LedAstray = LedAstray TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ledAstray :: TreacheryCard LedAstray
ledAstray = treachery LedAstray Cards.ledAstray

instance RunMessage LedAstray where
  runMessage msg t@(LedAstray attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cultists <- selectListMap EnemyTarget $ EnemyWithTrait Cultist
      hasNoClues <- fieldP InvestigatorClues (== 0) iid
      let
        advanceAgenda = [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
        placeDoomOnCultist target =
          TargetLabel
            target
            [InvestigatorSpendClues iid 1, PlaceClues (toSource attrs) target 1]
      player <- getPlayer iid
      pushAll
        $ if null cultists || hasNoClues
          then advanceAgenda
          else
            [ chooseOne
                player
                [ Label
                    "Place 1 of your clues on a Cultist enemy"
                    [chooseOne player $ map placeDoomOnCultist cultists]
                , Label
                    "Place 1 doom on the current agenda (this effect may cause the current agenda to advance)"
                    advanceAgenda
                ]
            ]
      pure t
    _ -> LedAstray <$> runMessage msg attrs
