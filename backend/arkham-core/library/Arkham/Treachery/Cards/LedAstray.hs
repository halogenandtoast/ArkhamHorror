module Arkham.Treachery.Cards.LedAstray
  ( ledAstray
  , LedAstray(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype LedAstray = LedAstray TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ledAstray :: TreacheryCard LedAstray
ledAstray = treachery LedAstray Cards.ledAstray

instance RunMessage LedAstray where
  runMessage msg t@(LedAstray attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cultists <- selectListMap EnemyTarget $ EnemyWithTrait Cultist
      hasClues <- fieldP InvestigatorClues (> 0) iid
      let
        advanceAgenda = [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
        placeDoomOnCultist target =
          TargetLabel target [InvestigatorSpendClues iid 1, PlaceClues target 1]
        revelation = if null cultists || hasClues
          then advanceAgenda
          else
            [ chooseOne
                iid
                [ Label
                  "Place 1 of your clues on a Cultist enemy"
                  [chooseOne iid $ map placeDoomOnCultist cultists]
                , Label
                  "Place 1 doom on the current agenda (this effect may cause the current agenda to advance)"
                  advanceAgenda
                ]
            ]
      t <$ pushAll (revelation <> [Discard (toSource attrs) $ toTarget attrs])
    _ -> LedAstray <$> runMessage msg attrs
