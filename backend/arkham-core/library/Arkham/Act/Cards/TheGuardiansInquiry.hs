module Arkham.Act.Cards.TheGuardiansInquiry (
  TheGuardiansInquiry (..),
  theGuardiansInquiry,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Placement

newtype TheGuardiansInquiry = TheGuardiansInquiry ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardiansInquiry :: ActCard TheGuardiansInquiry
theGuardiansInquiry =
  act (1, E) TheGuardiansInquiry Cards.theGuardiansInquiry
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Northside"

instance RunMessage TheGuardiansInquiry where
  runMessage msg a@(TheGuardiansInquiry attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      mariaDeSilva <- genCard Assets.mariaDeSilva
      curiositieShoppe <- selectJust $ LocationWithTitle "Curiositie Shoppe"
      assetId <- getRandom
      pushAll
        [ CreateAssetAt assetId mariaDeSilva (AtLocation curiositieShoppe)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> TheGuardiansInquiry <$> runMessage msg attrs
