module Arkham.Types.Location.Cards.HistoricalSocietyPeabodysOffice
  ( historicalSocietyPeabodysOffice
  , HistoricalSocietyPeabodysOffice(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype HistoricalSocietyPeabodysOffice = HistoricalSocietyPeabodysOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyPeabodysOffice :: LocationCard HistoricalSocietyPeabodysOffice
historicalSocietyPeabodysOffice = locationWithRevealedSideConnections
  HistoricalSocietyPeabodysOffice
  Cards.historicalSocietyPeabodysOffice
  4
  (PerPlayer 2)
  NoSymbol
  [Star]
  Moon
  [Star]

instance Query AssetMatcher env => HasModifiersFor env HistoricalSocietyPeabodysOffice where
  getModifiersFor _ (LocationTarget lid) (HistoricalSocietyPeabodysOffice attrs)
    | toId attrs == lid = do
      modifierIsActive <- notNull <$> select
        (assetIs Assets.mrPeabody
        <> AssetOwnedBy (InvestigatorAt $ LocationWithId lid)
        )
      pure $ toModifiers attrs [ ShroudModifier (-2) | modifierIsActive ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HistoricalSocietyPeabodysOffice where
  getAbilities (HistoricalSocietyPeabodysOffice attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
        Timing.When
        (LocationWithId $ toId attrs)
        AnyEnemy
    | not (locationRevealed attrs)
    ]


instance LocationRunner env => RunMessage env HistoricalSocietyPeabodysOffice where
  runMessage msg l@(HistoricalSocietyPeabodysOffice attrs) = case msg of
    UseCardAbility _ source _ 1 _
      | isSource attrs source && not (locationRevealed attrs) -> l
      <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyPeabodysOffice <$> runMessage msg attrs
