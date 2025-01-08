module Arkham.Location.Cards.HistoricalSocietyPeabodysOffice (
  historicalSocietyPeabodysOffice,
  HistoricalSocietyPeabodysOffice (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg

newtype HistoricalSocietyPeabodysOffice = HistoricalSocietyPeabodysOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyPeabodysOffice :: LocationCard HistoricalSocietyPeabodysOffice
historicalSocietyPeabodysOffice =
  location
    HistoricalSocietyPeabodysOffice
    Cards.historicalSocietyPeabodysOffice
    4
    (PerPlayer 2)

instance HasModifiersFor HistoricalSocietyPeabodysOffice where
  getModifiersFor (HistoricalSocietyPeabodysOffice a) =
    modifySelfWhenM
      a
      (selectAny $ assetIs Assets.mrPeabody <> AssetControlledBy (investigatorAt a))
      [ShroudModifier (-2)]

instance HasAbilities HistoricalSocietyPeabodysOffice where
  getAbilities (HistoricalSocietyPeabodysOffice attrs) =
    extend
      attrs
      [ mkAbility attrs 1 $ forced $ EnemySpawns #when (be attrs) AnyEnemy
      | attrs.unrevealed
      ]

instance RunMessage HistoricalSocietyPeabodysOffice where
  runMessage msg l@(HistoricalSocietyPeabodysOffice attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyPeabodysOffice <$> liftRunMessage msg attrs
