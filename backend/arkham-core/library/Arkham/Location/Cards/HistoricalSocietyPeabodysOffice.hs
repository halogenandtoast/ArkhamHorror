module Arkham.Location.Cards.HistoricalSocietyPeabodysOffice (
  historicalSocietyPeabodysOffice,
  HistoricalSocietyPeabodysOffice (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyPeabodysOffice = HistoricalSocietyPeabodysOffice LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyPeabodysOffice :: LocationCard HistoricalSocietyPeabodysOffice
historicalSocietyPeabodysOffice =
  location
    HistoricalSocietyPeabodysOffice
    Cards.historicalSocietyPeabodysOffice
    4
    (PerPlayer 2)

instance HasModifiersFor HistoricalSocietyPeabodysOffice where
  getModifiersFor (LocationTarget lid) (HistoricalSocietyPeabodysOffice attrs)
    | toId attrs == lid = do
        modifierIsActive <-
          notNull
            <$> select
              ( assetIs Assets.mrPeabody
                  <> AssetControlledBy (InvestigatorAt $ LocationWithId lid)
              )
        pure $ toModifiers attrs [ShroudModifier (-2) | modifierIsActive]
  getModifiersFor _ _ = pure []

instance HasAbilities HistoricalSocietyPeabodysOffice where
  getAbilities (HistoricalSocietyPeabodysOffice attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ ForcedAbility
        $ EnemySpawns
          Timing.When
          (LocationWithId $ toId attrs)
          AnyEnemy
      | not (locationRevealed attrs)
      ]

instance RunMessage HistoricalSocietyPeabodysOffice where
  runMessage msg l@(HistoricalSocietyPeabodysOffice attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source && not (locationRevealed attrs) ->
          l
            <$ push (Msg.RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyPeabodysOffice <$> runMessage msg attrs
