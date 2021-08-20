module Arkham.Types.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica = locationWith
  SanMarcoBasilica
  Cards.sanMarcoBasilica
  3
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

ability :: LocationAttrs -> Ability
ability a = mkAbility a 1 (ActionAbility Nothing $ ActionCost 1)

instance HasAbilities env SanMarcoBasilica where
  getAbilities iid window@(Window Timing.When NonFast) (SanMarcoBasilica attrs)
    = withBaseActions iid window attrs $ pure [ability attrs]
  getAbilities iid window (SanMarcoBasilica attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env SanMarcoBasilica where
  runMessage msg l@(SanMarcoBasilica attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      innocentRevelerIds <-
        selectList $ AssetOwnedBy You <> assetIs Assets.innocentReveler
      l <$ push
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget innocentRevelerId)
              [ PlaceUnderneath
                ActDeckTarget
                [ PlayerCard $ lookupPlayerCard
                    Assets.innocentReveler
                    (unAssetId innocentRevelerId)
                ]
              , RemoveFromGame (AssetTarget innocentRevelerId)
              ]
          | innocentRevelerId <- innocentRevelerIds
          ]
        )
    _ -> SanMarcoBasilica <$> runMessage msg attrs
